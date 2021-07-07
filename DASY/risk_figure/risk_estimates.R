# Risk estimates to run in parallel

library(stars)
library(sf)
library(purrr)
library(dplyr)
library(glue)
library(tidycensus)
library(furrr)
library(readr)

# Setup parallel
options(mc.cores = 8)
plan(multicore)

# Read raster data
countydata <- read_csv('/nfs/qread-data/DASY/countycodesforfb.csv', col_types = 'ccc')
tifpath <- '/nfs/qread-data/DASY/countyrasters'

wildfire_rasters <- future_map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_wildfire.tif')))
ourdasy_rasters <- future_map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_dasy.tif')))
epadasy_rasters <- future_map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_epadasy.tif')))
fbpop_rasters <- future_map(countydata$FIPS, ~ read_stars(glue('{tifpath}/county{.}_fblonglat.tif')))

# Download block group 2016 population estimates
get_blockgroup_pop <- function(FIPS, crs) {
  
  census_api_key(readLines('/research-home/qread/censusapikey.txt')) 
  
  pop <- get_acs(geography = "block group", variables = "B01003_001", 
                 year = 2016, state= substr(FIPS, 1, 2), county = substr(FIPS, 3, 5), 
                 geometry = TRUE)   
  
  # Data QC: remove empty geometries from pop
  pop <- pop[!is.na(st_dimension(pop)), ]
  
  st_transform(pop, crs = crs)
}

crs_wildfire <- st_crs(wildfire_rasters[[1]])
blockgroup_pops <- future_map(countydata$FIPS, get_blockgroup_pop, crs = crs_wildfire)

# Polygonize wildfire raster
wildfire_polygons <- future_map(wildfire_rasters, st_as_sf, as_points = FALSE, merge = TRUE)

# Convert polygons to each of the rasters' coordinate systems
wildfire_polygons_ourdasycrs <- future_map(wildfire_polygons, st_transform, crs = st_crs(ourdasy_rasters[[1]]))
wildfire_polygons_epadasycrs <- future_map(wildfire_polygons, st_transform, crs = st_crs(epadasy_rasters[[1]]))
wildfire_polygons_fbpopcrs <- future_map(wildfire_polygons, st_transform, crs = st_crs(fbpop_rasters[[1]]))

# Correct invalid geometries in the lat-long polygons
wildfire_polygons_fbpopcrs <- future_map(wildfire_polygons_fbpopcrs, st_make_valid)

# Get population totals by polygon
wildfire_sums_ourdasy <- future_map2(ourdasy_rasters, wildfire_polygons_ourdasycrs, aggregate, FUN = sum, na.rm = TRUE)
wildfire_sums_epadasy <- future_map2(epadasy_rasters, wildfire_polygons_epadasycrs, aggregate, FUN = sum, na.rm = TRUE)
wildfire_sums_fbpop <- future_map2(fbpop_rasters, wildfire_polygons_fbpopcrs, aggregate, FUN = sum, na.rm = TRUE)

# Get population totals by wildfire class for blockgroups
table_by_polygon <- function(env_raster, bg_pop_poly) {
  map(st_geometry(bg_pop_poly), ~ as.data.frame(table(st_crop(env_raster, .)[[1]], useNA = 'always')))
}

wildfire_tables_blockgroups <- future_map2(wildfire_rasters, blockgroup_pops, table_by_polygon)

sum_classes_blockgroup <- function(pop_table, bg_poly) {
  map_dfr(1:length(pop_table), ~ data.frame(st_drop_geometry(bg_poly[., c('GEOID', 'estimate')]), pop_table[[.]])) %>%
    group_by(GEOID) %>%
    mutate(pop = estimate * Freq / sum(Freq))
}

wildfire_sums_blockgroups <- future_map2(wildfire_tables_blockgroups, blockgroup_pops, sum_classes_blockgroup)

save(wildfire_sums_blockgroups, wildfire_sums_epadasy, wildfire_sums_ourdasy, wildfire_sums_fbpop, file = '/nfs/qread-data/DASY/wildfire_sums_temp.RData')

# Get grand totals by class for each county
grandtotal_classes <- function(env_poly, pop_poly) {
  data.frame(env_class = env_poly[[1]], pop = pop_poly[[1]]) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop))
}

wildfire_grandtotals_ourdasy <- map2(wildfire_polygons_ourdasycrs, wildfire_sums_ourdasy, grandtotal_classes)
wildfire_grandtotals_epadasy <- map2(wildfire_polygons_epadasycrs, wildfire_sums_epadasy, grandtotal_classes)
wildfire_grandtotals_fbpop <- map2(wildfire_polygons_fbpopcrs, wildfire_sums_fbpop, grandtotal_classes)

wildfire_grandtotals_blockgroups <- map(wildfire_sums_blockgroups, function(dat) {
  dat %>%
    rename(env_class = Var1) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop))
})

make_long_data <- function(grandtotals, estimate_name) {
  countydata %>%
    mutate(estimate = estimate_name) %>%
    mutate(listcol = grandtotals) %>%
    tidyr::unnest(cols = listcol)
}

wildfire_risks <- rbind(
  make_long_data(wildfire_grandtotals_ourdasy, 'our dasymetric'),
  make_long_data(wildfire_grandtotals_epadasy, 'EPA dasymetric'),
  make_long_data(wildfire_grandtotals_fbpop, 'Facebook pop map'),
  make_long_data(wildfire_grandtotals_blockgroups, 'Census block group equal weighting')
)

write_csv(wildfire_risks, '/nfs/qread-data/DASY/wildfire_risk_totals.csv')

