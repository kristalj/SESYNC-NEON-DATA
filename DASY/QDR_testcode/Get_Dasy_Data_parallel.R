### Script to run Get_Dasy_Data directly through slurm **NOT rslurm**
### QDR 13 Nov 2020

# =========================
# BEGIN FUNCTION DEFINITION
# =========================

Get_Dasy_Data <- function(stid, ctyid){

  census_api_key(readLines('/nfs/rswanwick-data/rswanwick_census_api_key.txt')) 
  # This is done to not have the API key in your environment or scripts (good practice)

  pop <- get_acs(geography = "block group", variables = "B00001_001", 
                 year = 2016, state= stid, county = ctyid, 
                 geometry = TRUE)   
  
  # Data QC: remove empty geometries from pop
  pop <- pop[!is.na(st_dimension(pop)), ]
  
  #download land use data NEED TO MAKE SURE WE DON"T HAVE TO HAVE PROJECTIONS MATCHING BEFOREHAND
  # Set an extraction data directory so that we don't have multiple tasks downloading to the same directory,.
  nlcd_download_path <- file.path(tempdir(), paste('nlcd', stid, ctyid, sep = '_'))
  lu <- get_nlcd(template = pop, label = paste0(stid, ctyid),year = 2016, dataset = "Impervious", extraction.dir = nlcd_download_path)
  
  #download 2010 block-level data, filter for only the blocks with 0 pop
  zero.pop <- get_decennial(geography = "block", variables = "P001001", 
                            year = 2010, state = stid, county = ctyid, 
                            geometry = TRUE) %>% filter(value == 0) %>% st_transform(., proj4string(lu))
  
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
  
  #this is where will put the file path for rswanwick public data 
  my_filename = as.character(glue("/nfs/rswanwick-data/DASY/tifs/neon-dasy-{stid}-{ctyid}.tif"))
  
  writeRaster(dasy.pop, my_filename, overwrite = TRUE) # Will overwrite existing file with the same name.
  
  message(glue("saved raster with stid {stid} and ctyid {ctyid}. Onto the next one!"))
  
}

# =======================
# END FUNCTION DEFINITION
# =======================

# Run function in parallel
# ========================

# Load packages
library(tidycensus)
library(FedData) #has to be dev version or won't dl 2016 data
library(tidyverse)
library(raster)
library(sf)
library(glue)
library(furrr) 

options(mc.cores = 8) # Use all eight cores on a node.
plan(multicore) # Sets up parallel execution of function.

# Test a Slurm job with a small number of counties.

#get the fips codes that the cluster can run through 
fipscodes = data_frame(fips_codes)

fipscodes = fipscodes %>% 
  rename(
    stid = state_code, 
    ctyid = county_code
  ) %>% dplyr::select(stid, ctyid)

#run through a subset of the fips codes for a sample (20 counties in MD)
fipscodes_mini <- fipscodes[fipscodes$stid == "24", ][1:20,]

### Execute Get_Dasy_Data in parallel across the subset of fips codes.
future_pmap(fipscodes_mini, Get_Dasy_Data)
          