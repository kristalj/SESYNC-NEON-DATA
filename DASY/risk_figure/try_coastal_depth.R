# Test whether the coastal flooding depth raster can be used with AA county

library(stars)
library(sf)
library(purrr)
library(dplyr)
library(glue)
library(tidycensus)
library(readr)
library(ggplot2)

countydata <- read_csv('/nfs/qread-data/DASY/countycodesforfb.csv', col_types = 'ccc')
tifpath <- '/nfs/qread-data/DASY/countyrasters'

aa <- countydata$FIPS[1]

ourdasy_aa <- read_stars(glue('{tifpath}/county{aa}_dasy.tif'))
epadasy_aa <- read_stars(glue('{tifpath}/county{aa}_epadasy.tif'))
fbpop_aa <- read_stars(glue('{tifpath}/county{aa}_fblonglat.tif'))
huang_aa <- read_stars(glue('{tifpath}/county{aa}_huangdasy.tif'))

cstdepth_aa <- read_stars('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/CstDpth01pct.tif')

get_blockgroup_pop <- function(FIPS, crs) {
  
  census_api_key(readLines('/research-home/qread/censusapikey.txt')) 
  
  pop <- get_acs(geography = "block group", variables = "B01003_001", 
                 year = 2016, state= substr(FIPS, 1, 2), county = substr(FIPS, 3, 5), 
                 geometry = TRUE)   
  
  # Data QC: remove empty geometries from pop
  pop <- pop[!is.na(st_dimension(pop)), ]
  
  st_transform(pop, crs = crs)
}

crs_cstdepth <- st_crs(cstdepth_aa)
blockgroup_pop_aa <- get_blockgroup_pop(aa, crs = crs_cstdepth)

cstdepth_ourdasycrs <- st_transform(cstdepth_aa, crs = st_crs(ourdasy_aa))
cstdepth_epadasycrs <- st_transform(cstdepth_aa, crs = st_crs(epadasy_aa))

# Just get the number of people living in the area with non-NA pixels
# Convert this to polygon
cstdepth_aa_notna <- !is.na(cstdepth_aa)
cstdepth_aa_notna_poly <- st_as_sf(cstdepth_aa_notna, as_points = FALSE, merge = TRUE)

cstdepth_aa_notna_poly_ourdasycrs <- st_transform(cstdepth_aa_notna_poly, crs = st_crs(ourdasy_aa))
cstdepth_aa_notna_poly_epadasycrs <- st_transform(cstdepth_aa_notna_poly, crs = st_crs(epadasy_aa))
cstdepth_aa_notna_poly_fbpopcrs <- st_transform(cstdepth_aa_notna_poly, crs = st_crs(fbpop_aa))

cstdepth_aa_sums <- aggregate(ourdasy_aa, cstdepth_aa_notna_poly_ourdasycrs, FUN = sum, na.rm = TRUE)

grandtotal_classes <- function(env_poly, pop_raster) {
  data.frame(env_class = env_poly[[1]], pop = pop_raster[[1]]) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
}

cstdepth_aa_grandtotals_ourdasy <- grandtotal_classes(cstdepth_aa_notna_poly_ourdasycrs, cstdepth_aa_sums)
