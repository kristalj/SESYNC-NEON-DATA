# Test accuracy of code by projecting everything to equal area and comparing results to the web mercator projection (default)
# QDR 10 Dec 2020

# Test on Anne Arundel County

stid <- '24'
ctyid <- '003'

library(tidycensus)
library(FedData) 
library(tidyverse)
library(raster)
library(sf)
library(glue)

census_api_key(readLines('/nfs/rswanwick-data/rswanwick_census_api_key.txt')) 
# This is done to not have the API key in your environment or scripts (good practice)

pop <- get_acs(geography = "block group", variables = "B00001_001", 
               year = 2016, state= stid, county = ctyid, 
               geometry = TRUE)   

# Data QC: remove empty geometries from pop
pop <- pop[!is.na(st_dimension(pop)), ]

#download land use data NEED TO MAKE SURE WE DON"T HAVE TO HAVE PROJECTIONS MATCHING BEFOREHAND
# Set an extraction data directory so that we don't have multiple tasks downloading to the same directory.
# Instead of using tempdir() use a temporary directory I created for the purpose. This might avoid permissions issues.
nlcd_download_path <- file.path('/nfs/rswanwick-data/DASY/temp_files', paste('nlcd', stid, ctyid, sep = '_'))
lu <- get_nlcd(template = pop, label = paste0(stid, ctyid),year = 2016, dataset = "Impervious", extraction.dir = nlcd_download_path)

#download 2010 block-level data, filter for only the blocks with 0 pop
zero.pop <- get_decennial(geography = "block", variables = "P001001", 
                          year = 2010, state = stid, county = ctyid, 
                          geometry = TRUE) %>% filter(value == 0) %>% st_transform(., proj4string(lu))

##### Original (web mercator) result

pop.projected <- st_transform(pop, crs = proj4string(lu))
##crop lu to county
lu.crop <- crop(lu, pop.projected)
lu.mask <- mask(lu.crop, pop.projected)
#Remove NLCD data <=1%
lu.mask[lu.mask <= 1] <- NA

#create lu ratio
lu.ratio <- lu.mask/100

#mask out zero pop blocks
lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse=TRUE)

#get the impervious surface descriptor dataset from: https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3AUrban%20Imperviousness&f%5B2%5D=year%3A2016
# Now VRT is used.
imp.surf.desc <- raster("/nfs/rswanwick-data/DASY/NLCD_2016_impervious.vrt")
#mask out primary, secondary, and urban tertiary roads
imp.surf.crop <- raster::crop(imp.surf.desc, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #crop imp surface to county
#plot(imp.surf.crop)
imp.surf.mask <- raster::mask(imp.surf.crop, spTransform(as(pop.projected, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #mask all non-county values to NA

# Correct for zero to one based indexing by adding 1 to the raster
imp.surf.mask <- imp.surf.mask + 1

reclass.table <- matrix(c(1,6,1,7,14,NA), ncol=3) #reclassify values 1-6 into 1 for keep drop the rest

imp.roads <- reclassify(imp.surf.mask, reclass.table)
imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp)#have to reproject the descriptor file
#Mask out roads (i.e, all NonNA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

#get the block-group level sum of the remaining impervious surface pixels
RISA.sum <- raster::extract(RISA, as(pop.projected,"Spatial"), fun=sum, na.rm=TRUE,df=TRUE)

pop.df <- cbind(pop.projected, RISA.sum$layer)
bg.sum.pop <- fasterize::fasterize(pop.projected, RISA, field = "estimate")
bg.sum.RISA <- fasterize::fasterize(pop.df, RISA, field = "RISA.sum.layer")

#generate density (people/30 m pixel)
dasy.pop <- (bg.sum.pop/bg.sum.RISA) * RISA

##### Modified (Albers equal area) result

# Project everything to AEA
aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

pop.aea <- st_transform(pop, crs = aea)
lu.aea <- projectRaster(lu, crs = aea)
zero.pop.aea <- st_transform(zero.pop, crs = aea)
# Continue as before:

##crop lu to county
lu.crop <- crop(lu.aea, pop.aea)
lu.mask <- mask(lu.crop, pop.aea)
#Remove NLCD data <=1%
lu.mask[lu.mask <= 1] <- NA

#create lu ratio
lu.ratio <- lu.mask/100

#mask out zero pop blocks
lu.ratio.zp <- mask(lu.ratio, as(zero.pop.aea, "Spatial"), inverse=TRUE)

#get the impervious surface descriptor dataset from: https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3AUrban%20Imperviousness&f%5B2%5D=year%3A2016
# Now VRT is used.
imp.surf.desc <- raster("/nfs/rswanwick-data/DASY/NLCD_2016_impervious.vrt")
#mask out primary, secondary, and urban tertiary roads
imp.surf.crop <- raster::crop(imp.surf.desc, spTransform(as(pop.aea, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #crop imp surface to county
#plot(imp.surf.crop)
imp.surf.mask <- raster::mask(imp.surf.crop, spTransform(as(pop.aea, "Spatial"), CRSobj = proj4string(imp.surf.desc))) #mask all non-county values to NA

# Correct for zero to one based indexing by adding 1 to the raster
imp.surf.mask <- imp.surf.mask + 1

reclass.table <- matrix(c(1,6,1,7,14,NA), ncol=3) #reclassify values 1-6 into 1 for keep drop the rest

imp.roads <- reclassify(imp.surf.mask, reclass.table)
imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp)#have to reproject the descriptor file
#Mask out roads (i.e, all NonNA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

#get the block-group level sum of the remaining impervious surface pixels
RISA.sum <- raster::extract(RISA, as(pop.aea,"Spatial"), fun=sum, na.rm=TRUE,df=TRUE)

pop.df <- cbind(pop.aea, RISA.sum$layer)
bg.sum.pop <- fasterize::fasterize(pop.aea, RISA, field = "estimate")
bg.sum.RISA <- fasterize::fasterize(pop.df, RISA, field = "RISA.sum.layer")

#generate density (people/30 m pixel)
dasy.pop.aea <- (bg.sum.pop/bg.sum.RISA) * RISA


# Extract population density at a coordinate the same for both

annap <- SpatialPoints(coords = cbind(-76.49, 38.98), proj4string = CRS("+init=epsg:4326"))

(annap_dasy_mercator <- extract(dasy.pop, annap))
(annap_dasy_albers <- extract(dasy.pop.aea, annap))

# relative error
(annap_dasy_mercator - annap_dasy_albers) / annap_dasy_albers # 1% error.

annap_box <- as(raster::extent(-76.51, -76.47, 38.96, 39.0), "SpatialPolygons")
proj4string(annap_box) <- "+init=epsg:4326"

annapbox_dasy_mercator <- extract(dasy.pop, annap_box)
annapbox_dasy_albers <- extract(dasy.pop.aea, annap_box)

# relative error
meanboxmerc <- mean(annapbox_dasy_mercator[[1]], na.rm = TRUE)
meanboxalb <- mean(annapbox_dasy_albers[[1]], na.rm = TRUE)
(meanboxmerc - meanboxalb) / meanboxalb # 10% error.
