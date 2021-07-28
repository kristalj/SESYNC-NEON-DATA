# Tally up flood risk pixels for each dasymetric estimate for each country
# QDR / 27 June 2021
library(stars)
library(sf)
library(purrr)
library(dplyr)
library(glue)
library(tidycensus)
library(readr)
library(ggplot2)
library(raster)

countydata <- read_csv('/nfs/qread-data/DASY/countycodesforfb.csv', col_types = 'ccc')
tifpath <- '/nfs/qread-data/DASY/countyrasters'
wsepath <- '/nfs/qread-data/DASY/wserasters'
dasypath <- '/nfs/qread-data/DASY/tifs'

#wse_raster <- raster('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/WSE_01pct.tif')

wse_rasters <- map(countydata$FIPS, ~ read_stars(glue('FIXMEFIXME'))) # FIXME This probably needs to be done with raster()
ourdasy_rasters <- map(countydata$FIPS, ~ read_stars(glue('{dasypath}/neon-dasy-{substr(., 1, 2)}-{substr(., 3, 5)}.tif')))
epadasy_rasters <- map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_epadasy.tif')))
huang_rasters <- map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_huangdasy.tif')))
fbpop_rasters <- map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_fblonglat.tif')))

get_blockgroup_pop <- function(FIPS, crs) {
  
  census_api_key(readLines('/research-home/qread/censusapikey.txt')) 
  
  pop <- get_acs(geography = "block group", variables = "B01003_001", 
                 year = 2016, state= substr(FIPS, 1, 2), county = substr(FIPS, 3, 5), 
                 geometry = TRUE)   
  
  # Data QC: remove empty geometries from pop
  pop <- pop[!is.na(st_dimension(pop)), ]
  
  st_transform(pop, crs = crs)
}

crs_wse <- st_crs(wse_raster)
blockgroup_pops <- map(countydata$FIPS, get_blockgroup_pop, crs = crs_wse)

wse_notna_raster <- map(wse_rasters, ~ st_as_stars(!is.na(.)))
wse_notna_poly <- map(wse_rasters, st_as_sf, as_points = FALSE, merge = TRUE)

wse_notna_poly_ourdasycrs <- map(wse_notna_poly, st_transform, crs = st_crs(ourdasy_rasters[[1]]))
wse_notna_poly_epadasycrs <- map(wse_notna_poly, st_transform, crs = st_crs(epadasy_rasters[[1]]))
wse_notna_poly_huangcrs <- map(wse_notna_poly, st_transform, crs = st_crs(huang_rasters[[1]]))
wse_notna_poly_fbpopcrs <- map(wse_notna_poly, st_transform, crs = st_crs(fbpop_rasters[[1]])) %>%
  map(st_make_valid)

wse_sums_ourdasy <- map2(ourdasy_rasters, wse_notna_poly_ourdasycrs, aggregate, FUN = sum, na.rm = TRUE)
wse_sums_epadasy <- map2(epadasy_rasters, wse_notna_poly_epadasycrs, aggregate, FUN = sum, na.rm = TRUE)
wse_sums_huang <- map2(huang_rasters, wse_notna_poly_huangcrs, aggregate, FUN = sum, na.rm = TRUE)
wse_sums_fbpop <- map2(fbpop_rasters, wse_notna_poly_fbpopcrs, aggregate, FUN = sum, na.rm = TRUE)

# For blockgroups, instead of aggregating pop raster by flood polygon, instead intersect blockgroup polygon and flood polygon.
get_blockgroup_sums <- function(bg_pop, env_poly, env_crs) {
  bg_pop %>%
    st_transform(env_crs) %>%
    mutate(area = st_area(.)) %>%
    st_intersection(env_poly) %>%
    mutate(area_int = st_area(.))
}

wse_sums_blockgroup <- map2(blockgroup_pops, wse_notna_poly, env_crs = st_crs(wse_notna_raster[[1]]))

grandtotal_classes <- function(env_poly, pop_raster) {
  data.frame(env_class = env_poly[[1]], pop = pop_raster[[1]]) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
}

wse_grandtotals_ourdasy <- map2(wse_notna_poly_ourdasycrs, wse_sums_ourdasy, grandtotal_classes)
wse_grandtotals_epadasy <- map2(wse_notna_poly_epadasycrs, wse_sums_epadasy, grandtotal_classes)
wse_grandtotals_huang <- map2(wse_notna_poly_huangcrs, wse_sums_huang, grandtotal_classes)
wse_grandtotals_fbpop <- map2(wse_notna_poly_fbpopcrs, wse_sums_fbpop, grandtotal_classes)

get_blockgroup_grandtotals <- function(env_sums) {
  env_sums %>%
    st_drop_geometry() %>%
    rename(env_class = layer) %>%
    group_by(env_class) %>%
    summarize(pop = as.numeric(sum(estimate * area_int/area)))
}

wse_grandtotals_blockgroup <- map(wse_sums_blockgroup, get_blockgroup_grandtotals)

# Extract the number of people in the WSE 1% flood area, as estimated by each of the five methods.
# FIXME this needs to be edited for the list of counties.
make_long_data <- function(grandtotals, estimate_name) {
  countydata %>%
    as_tibble %>%
    mutate(estimate = estimate_name) %>%
    mutate(listcol = grandtotals) %>%
    tidyr::unnest(cols = listcol)
}

flood_risks <- rbind(
  make_long_data(wse_grandtotals_ourdasy, 'our dasymetric'),
  make_long_data(wse_grandtotals_epadasy, 'EPA dasymetric'),
  make_long_data(wse_grandtotals_epadasy, 'Huang et al. population grid'),
  make_long_data(wse_grandtotals_fbpop, 'Facebook pop map'),
  make_long_data(wse_grandtotals_blockgroups, 'Census block group equal weighting')
)

# Save results
write_csv(flood_risks, '/nfs/qread-data/DASY/flood_risk_totals.csv')

# reshape
flood_risks_wide <- flood_risks %>%
  group_by(estimate, county_name) %>%
  mutate(prop = pop/sum(pop),
         env_class = c('not_at_risk', 'at_risk')[env_class+1]) %>%
  tidyr::pivot_wider(names_from = env_class, values_from = c(pop, prop))

# Make a fig
flood_risks %>%
  group_by(estimate, county_name) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(env_class == 1) %>%
  ggplot(aes(x = method, y = prop)) +
    geom_col() +
    scale_y_continuous(name = '% population in 1% flooding event zone', expand = expansion(c(0, 0.01)), labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))


