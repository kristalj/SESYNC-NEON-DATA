# Read flooding raster 
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

aa <- countydata$FIPS[1]

ourdasy_aa <- read_stars(glue('{tifpath}/county{aa}_dasy.tif'))
epadasy_aa <- read_stars(glue('{tifpath}/county{aa}_epadasy.tif'))
fbpop_aa <- read_stars(glue('{tifpath}/county{aa}_fblonglat.tif'))
huang_aa <- read_stars(glue('{tifpath}/county{aa}_huangdasy.tif'))

wse_raster <- raster('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/WSE_01pct.tif')

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
blockgroup_pop_aa <- get_blockgroup_pop(aa, crs = crs_wse)

wse_raster_notna <- !is.na(wse_raster) %>% st_as_stars
wse_raster_notna_poly <- st_as_sf(wse_raster_notna, as_points = FALSE, merge = TRUE)

wse_raster_notna_poly_ourdasycrs <- st_transform(wse_raster_notna_poly, crs = st_crs(ourdasy_aa))
wse_raster_notna_poly_epadasycrs <- st_transform(wse_raster_notna_poly, crs = st_crs(epadasy_aa))
wse_raster_notna_poly_fbpopcrs <- st_transform(wse_raster_notna_poly, crs = st_crs(fbpop_aa)) %>% st_make_valid
wse_raster_notna_poly_huangcrs <- st_transform(wse_raster_notna_poly, crs = st_crs(huang_aa))


wse_raster_ourdasy_sums <- aggregate(ourdasy_aa, wse_raster_notna_poly_ourdasycrs, FUN = sum, na.rm = TRUE)
wse_raster_epadasy_sums <- aggregate(epadasy_aa, wse_raster_notna_poly_epadasycrs, FUN = sum, na.rm = TRUE)
wse_raster_fbpop_sums <- aggregate(fbpop_aa, wse_raster_notna_poly_fbpopcrs, FUN = sum, na.rm = TRUE)
wse_raster_huang_sums <- aggregate(huang_aa, wse_raster_notna_poly_huangcrs, FUN = sum, na.rm = TRUE)

# For blockgroups, instead of aggregating pop raster by flood polygon, instead intersect blockgroup polygon and flood polygon.
blockgroup_wse_sums <- blockgroup_pop_aa %>%
  st_transform(st_crs(wse_raster_notna)) %>%
  mutate(area = st_area(.)) %>%
  st_intersection(wse_raster_notna_poly) %>%
  mutate(area_int = st_area(.))

grandtotal_classes <- function(env_poly, pop_raster) {
  data.frame(env_class = env_poly[[1]], pop = pop_raster[[1]]) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
}

wse_raster_grandtotals_ourdasy <- grandtotal_classes(wse_raster_notna_poly_ourdasycrs, wse_raster_ourdasy_sums)
wse_raster_grandtotals_epadasy <- grandtotal_classes(wse_raster_notna_poly_epadasycrs, wse_raster_epadasy_sums)
wse_raster_grandtotals_fbpop <- grandtotal_classes(wse_raster_notna_poly_fbpopcrs, wse_raster_fbpop_sums)
wse_raster_grandtotals_huang <- grandtotal_classes(wse_raster_notna_poly_huangcrs, wse_raster_huang_sums)

wse_raster_grandtotals_blockgroup <- blockgroup_wse_sums %>%
  st_drop_geometry() %>%
  rename(env_class = layer) %>%
  group_by(env_class) %>%
  summarize(pop = as.numeric(sum(estimate * area_int/area)))
  

# Extract the number of people in the WSE 1% flood area, as estimated by each of the five methods.

flood_risks <- map2_dfr(list(wse_raster_grandtotals_ourdasy, wse_raster_grandtotals_epadasy, wse_raster_grandtotals_fbpop, wse_raster_grandtotals_huang, wse_raster_grandtotals_blockgroup),
     c('our dasymetric', 'EPA dasymetric', 'Huang et al. population grid', 'Facebook pop map', 'Census block group equal weighting'),
    ~ tibble(method = .y, .x))

# Save results
write_csv(flood_risks, '/nfs/qread-data/DASY/flood_risk_totals_AA.csv')

# reshape
flood_risks %>%
  group_by(method) %>%
  mutate(prop = pop/sum(pop),
         env_class = c('not_at_risk', 'at_risk')[env_class+1]) %>%
  tidyr::pivot_wider(names_from = env_class, values_from = c(pop, prop))

# Make a fig
flood_risks %>%
  group_by(method) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(env_class == 1) %>%
  ggplot(aes(x = method, y = prop)) +
    geom_col() +
    scale_y_continuous(name = '% population in 1% flooding event zone', expand = expansion(c(0, 0.01)), labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave('/nfs/qread-data/DASY/figs/flood_risks_aa_county.png')  
