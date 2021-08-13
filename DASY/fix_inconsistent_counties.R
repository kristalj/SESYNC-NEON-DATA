# Dasymetric population estimation code
# Fix two inconsistent counties: Oglala Lakota, SD and Bedford, VA
# Quentin D. Read, last modified 13 August 2021

bad_counties <- data.frame(stid = c('46','46','51'),
                           ctyid = c('102', '113', '515'))

# Refer to pg. 3 of this documentation to see what happened to those counties since 2010:
# https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf

# What are the names listed in the fips_codes data frame for those counties?

fips_codes %>% filter(paste(state_code, county_code) %in% paste(bad_counties$stid, bad_counties$ctyid))
# Oglala Lakota County and Shannon County, SD; Bedford City, VA

# between 2010 and 2016, county 46113 (Shannon) was replaced by county 46102 (Oglala Lakota)
# So the 2010 data exists for county 46113, but not 2016. The opposite is true for county 46102.

# Solution is to manually set it up to download 46113 data for 2010, then 46102 data for 2016.

# For Bedford City, it was an independent city with code 51515 until 2013, when it was incorporated into Bedford County (51019).
# So it has valid 2010 data but no data for 2016. 

# The solution here would be to use the 2016 data from 51019, and for 2010 merge the 51515 and 51019 data.

census_api_key(readLines('censusapikey.txt'))

# Manual fix for South Dakota ---------------------------------------------

# copied and pasted get_dasy_data and modified.
# 2016 data is new code.
stid <- "46"
ctyid <- "102"
pop <- get_acs(geography = "block group", variables = "B01003_001", 
               year = 2016, state= stid, county = ctyid, 
               geometry = TRUE)   

aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Data QC: remove empty geometries from pop
pop <- pop[!is.na(st_dimension(pop)), ]
pop.projected <- st_transform(pop, crs = aea)

# Still use 46102 for NLCD (2016)
nlcd_imp_vrt <- 'NLCD/VRTs/NLCD_2016_Impervious_L48_20190405.vrt'
temp_polygon_filename <- as.character(glue("DASY/temp_files/county-{stid}-{ctyid}.gpkg"))
temp_nlcdraster_filename <- as.character(glue("DASY/temp_files/countynlcd-{stid}-{ctyid}.tif"))
st_write(st_union(pop.projected), dsn = temp_polygon_filename, driver = 'GPKG')
gdalwarp(srcfile = nlcd_imp_vrt, dstfile = temp_nlcdraster_filename, cutline = temp_polygon_filename, crop_to_cutline = TRUE, tr = c(30, 30), dstnodata = "None")
lu <- raster(temp_nlcdraster_filename)

#download 2010 block-level data, filter for only the blocks with 0 pop
# Use old FIPS code
ctyid <- "113"
zero.pop <- get_decennial(geography = "block", variables = "P001001", 
                          year = 2010, state = stid, county = ctyid, 
                          geometry = TRUE) %>% filter(value == 0) %>% st_transform(., proj4string(lu))

#Remove NLCD data <=1% 
lu[lu <= 1] <- NA

#create lu ratio
lu.ratio <- lu/100

#mask out zero pop blocks
lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse=TRUE)

imp.surf.desc <- raster("NLCD/NLCD_2016_Impervious_descriptor_L48_20190405/NLCD_2016_impervious.vrt")
#mask out primary, secondary, and urban tertiary roads
imp.surf.crop <- raster::crop(imp.surf.desc, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #crop imp surface to county
#plot(imp.surf.crop)
imp.surf.mask <- raster::mask(imp.surf.crop, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #mask all non-county values to NA

reclass.table <- matrix(c(1,6,1,7,14,NA), ncol = 3, byrow = TRUE) # reclassify values 1-6 into 1 for keep, drop the rest

imp.roads <- reclassify(imp.surf.mask, reclass.table, right = NA)
imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') # have to reproject the descriptor file
#Mask out roads (i.e, all NonNA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[!is.na(y[])] <- NA
  return(x)
})

#get the block-group level sum of the remaining impervious surface pixels
RISA.sum <- raster::extract(RISA, as(pop.projected,"Spatial"), fun=sum, na.rm=TRUE,df=TRUE)

pop.df <- cbind(pop.projected, RISA.sum$layer)
bg.sum.pop <- fasterize::fasterize(pop.projected, RISA, field = "estimate")
bg.sum.RISA <- fasterize::fasterize(pop.df, RISA, field = "RISA.sum.layer")

#generate density (people/30 m pixel)
dasy.pop <- (bg.sum.pop/bg.sum.RISA) * RISA

filename = as.character(glue("DASY/tifs/neon-dasy-{stid}-102.tif"))

writeRaster(dasy.pop, filename, overwrite = TRUE) # Will overwrite existing file with the same name.


# Manual fix for Virginia -------------------------------------------------

# Only one code used for 2016
stid <- "51"
ctyid <- "019"
pop <- get_acs(geography = "block group", variables = "B01003_001", 
               year = 2016, state= stid, county = ctyid, 
               geometry = TRUE)   

# Data QC: remove empty geometries from pop
pop <- pop[!is.na(st_dimension(pop)), ]
pop.projected <- st_transform(pop, crs = aea)

nlcd_imp_vrt <- 'NLCD/VRTs/NLCD_2016_Impervious_L48_20190405.vrt'
temp_polygon_filename <- as.character(glue("DASY/temp_files/county-{stid}-{ctyid}.gpkg"))
temp_nlcdraster_filename <- as.character(glue("DASY/temp_files/countynlcd-{stid}-{ctyid}.tif"))
st_write(st_union(pop.projected), dsn = temp_polygon_filename, driver = 'GPKG')
gdalwarp(srcfile = nlcd_imp_vrt, dstfile = temp_nlcdraster_filename, cutline = temp_polygon_filename, crop_to_cutline = TRUE, tr = c(30, 30), dstnodata = "None")
lu <- raster(temp_nlcdraster_filename)

#download 2010 block-level data, filter for only the blocks with 0 pop
# We need to get both the city and county and merge them.
zero.pop.bedfordcounty <- get_decennial(geography = "block", variables = "P001001", 
                          year = 2010, state = stid, county = ctyid, 
                          geometry = TRUE) %>% filter(value == 0) %>% st_transform(., proj4string(lu))
zero.pop.bedfordcity <- get_decennial(geography = "block", variables = "P001001", 
                                      year = 2010, state = stid, county = "515", 
                                      geometry = TRUE) %>% filter(value == 0) %>% st_transform(., proj4string(lu))

# Plot to check them

ggplot() +
  geom_sf(data = zero.pop.bedfordcounty, aes(fill = value), col = "blue") +
  geom_sf(data = zero.pop.bedfordcity, aes(fill = value), col = "red") 
# Looks good! There is a hole in the middle of the county filled in with the city.

zero.pop <- bind_rows(zero.pop.bedfordcity, zero.pop.bedfordcounty)

#Remove NLCD data <=1% 
lu[lu <= 1] <- NA

#create lu ratio
lu.ratio <- lu/100

#mask out zero pop blocks
lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse=TRUE)

imp.surf.desc <- raster("NLCD/NLCD_2016_Impervious_descriptor_L48_20190405/NLCD_2016_impervious.vrt")
imp.surf.crop <- raster::crop(imp.surf.desc, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #crop imp surface to county
#plot(imp.surf.crop)
imp.surf.mask <- raster::mask(imp.surf.crop, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #mask all non-county values to NA

reclass.table <- matrix(c(1,6,1,7,14,NA), ncol = 3, byrow = TRUE) # reclassify values 1-6 into 1 for keep, drop the rest

imp.roads <- reclassify(imp.surf.mask, reclass.table, right = NA)
imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') # have to reproject the descriptor file
#Mask out roads (i.e, all NonNA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[!is.na(y[])] <- NA
  return(x)
})

#get the block-group level sum of the remaining impervious surface pixels
RISA.sum <- raster::extract(RISA, as(pop.projected,"Spatial"), fun=sum, na.rm=TRUE,df=TRUE)

pop.df <- cbind(pop.projected, RISA.sum$layer)
bg.sum.pop <- fasterize::fasterize(pop.projected, RISA, field = "estimate")
bg.sum.RISA <- fasterize::fasterize(pop.df, RISA, field = "RISA.sum.layer")

#generate density (people/30 m pixel)
dasy.pop <- (bg.sum.pop/bg.sum.RISA) * RISA

filename = as.character(glue("DASY/tifs/neon-dasy-{stid}-{ctyid}.tif"))

writeRaster(dasy.pop, filename, overwrite = TRUE) # Will overwrite existing file with the same name.
