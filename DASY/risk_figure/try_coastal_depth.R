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

# Just get the number of people living in the area with non-NA pixels
# Convert this to polygon
cstdepth_ourdasy_notna <- !is.na(cstdepth_ourdasycrs)
cstdepth_ourdasy_notna_poly <- st_as_sf(cstdepth_ourdasy_notna, merge = TRUE, as_points = FALSE)
ourdasy_cstdepth <- ourdasy_aa[!is.na(cstdepth_ourdasycrs)]
