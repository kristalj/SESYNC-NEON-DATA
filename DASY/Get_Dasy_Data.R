### Clean version of Get_Dasy_Data
# QDR 12 Nov 2020
# QDR modified 10 Dec 2020 with these changes
# - replace get_nlcd() with extracting directly from the NLCD raster already on SESYNC's server
# - project everything to Albers equal-area before doing the computations so that area isn't distorted.
# QDR modified 12 July 2021
# - debugged the reclass table/ project raster part
# - changed filepaths so Q can run code
# QDR modified 24 August 2021
# - changed the masking for impervious <1%, zero-pop blocks, non-impervious, and road pixels to 0 instead of NA masks

# =========================
# BEGIN FUNCTION DEFINITION
# =========================

Get_Dasy_Data <- function(stid, ctyid){
  
  # Albers equal-area projection
  aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

  census_api_key(readLines('/research-home/qread/censusapikey.txt')) 
  # This is done to not have the API key in your environment or scripts (good practice)

  pop <- get_acs(geography = "block group", variables = "B01003_001", 
                 year = 2016, state= stid, county = ctyid, 
                 geometry = TRUE)   
  
  # Data QC: remove empty geometries from pop
  pop <- pop[!is.na(st_dimension(pop)), ]
  
  # Project population to Albers equal-area
  pop.projected <- st_transform(pop, crs = aea)
  
  # Update: use gdalwarp to extract the county area, from the NLCD impervious raster, already in Albers projection
  # Use a temporary directory I created for the purpose to write the county polygon for extraction. 
  nlcd_imp_vrt <- '/nfs/public-data/NLCD/VRTs/NLCD_2016_Impervious_L48_20190405.vrt'
  temp_polygon_filename <- as.character(glue("/nfs/qread-data/DASY/temp_files/county-{stid}-{ctyid}.gpkg"))
  temp_nlcdraster_filename <- as.character(glue("/nfs/qread-data/DASY/temp_files/countynlcd-{stid}-{ctyid}.tif"))
  st_write(st_union(pop.projected), dsn = temp_polygon_filename, driver = 'GPKG')
  gdalwarp(srcfile = nlcd_imp_vrt, dstfile = temp_nlcdraster_filename, cutline = temp_polygon_filename, crop_to_cutline = TRUE, tr = c(30, 30), dstnodata = "None")
  lu <- raster(temp_nlcdraster_filename)
  
  #download 2010 block-level data, filter for only the blocks with 0 pop
  zero.pop <- get_decennial(geography = "block", variables = "P001001", 
                            year = 2010, state = stid, county = ctyid, 
                            geometry = TRUE) %>% filter(value == 0) %>% st_transform(., aea)
  
  # Mask NLCD impervious raster to county boundaries
  lu <- mask(lu, as(pop.projected, "Spatial"))
  # Remove NLCD data <=1% (set to zero)
  lu[lu <= 1] <- 0
  
  #create lu ratio
  lu.ratio <- lu/100
  
  #mask out zero pop blocks (set to zero)
  lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse = TRUE, updatevalue = 0)
  
  #get the impervious surface descriptor dataset from: https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3AUrban%20Imperviousness&f%5B2%5D=year%3A2016
  # Now VRT is used.
  imp.surf.desc <- raster("/nfs/public-data/NLCD/NLCD_2016_Impervious_descriptor_L48_20190405/NLCD_2016_impervious.vrt")
  #mask out primary, secondary, and urban tertiary roads
  imp.surf.crop <- raster::crop(imp.surf.desc, as(pop.projected, "Spatial")) #crop imp surface to county
  #plot(imp.surf.crop)
  imp.surf.mask <- raster::mask(imp.surf.crop, as(pop.projected, "Spatial")) #mask all non-county values to NA

  reclass.table <- matrix(c(1,6,1,7,14,NA), ncol = 3, byrow = TRUE) # reclassify values 1-6 into 1 for keep, drop the rest
  
  imp.roads <- reclassify(imp.surf.mask, reclass.table, right = NA)
  imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') # have to reproject the descriptor file
  #Mask out roads (i.e, all NonNA values in imp.roads.p)
  RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
    x[!is.na(y[])] <- 0
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
  filename = as.character(glue("/nfs/qread-data/DASY/tifs/neon-dasy-{stid}-{ctyid}.tif"))
  
  writeRaster(dasy.pop, filename, overwrite = TRUE, NAflag = -9999) # Will overwrite existing file with the same name.
  
  message(glue("saved raster with stid {stid} and ctyid {ctyid}. Onto the next one!"))
  
}

# =======================
# END FUNCTION DEFINITION
# =======================



# Code to run full job on all counties
# ====================================

# Code written so that no two counties from the same state are being run at the same time.
# This will prevent two tasks from trying to access the same state-level files at the same time, which causes an error.

# Load packages
library(tidycensus)
library(tidyverse)
library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)

# Get the fips codes for all counties
fipscodes = data_frame(fips_codes)

fipscodes = fipscodes %>% 
  rename(
    stid = state_code, 
    ctyid = county_code
  ) %>% dplyr::select(stid, ctyid)

# Get rid of the ones that are not in the 48 continental states and DC
not48 <- c('02','15','60','66','69','72','74','78')
fipscodes <- filter(fipscodes, !stid %in% not48)

# Remove Shannon County, South Dakota (46113) from the list because it does not exist in 2016 (replaced by 46102, Oglala Lakota County)
# Also remove Bedford City, Virginia (51515) from the list because it does not exist in 2016 (merged into 51019, Bedford County)
fipscodes <- filter(fipscodes, !(stid == '46' & ctyid == '113') & !(stid == '51' & ctyid == '515'))

# Split into a list by state.
fips_list <- fipscodes %>% group_by(stid) %>% group_split

# Function to run all counties in a state
get_dasy_all_counties <- function(fips) {
  walk(fips$ctyid, ~ Get_Dasy_Data(stid = fips$stid[1], ctyid = .))
}

sjob <- slurm_map(fips_list, get_dasy_all_counties, jobname = 'DASYallcounties',
                  nodes = 7, cpus_per_node = 8, pkgs = c("tidycensus", "raster", "tidyverse", "sf", "dplyr", "glue", "gdalUtils"),
                  global_objects = "Get_Dasy_Data", 
                  submit = TRUE)

# Get output after job is done. This is just for diagnostics. 
joutput <- get_slurm_out(sjob)
# Run cleanup_files to delete the temporary diagnostic files.
cleanup_files(sjob)
# Also delete the temporarily downloaded files
# This is needed because now that we've created the file download directory manually, it does not automatically go away once the job finishes.
system2('rm', '-r /nfs/qread-data/DASY/temp_files/*') 

# Check which jobs did not run properly. This will be TRUE for all where the file was written as intended.
tif_exists <- file.exists(as.character(glue("/nfs/qread-data/DASY/tifs/neon-dasy-{fipscodes$stid}-{fipscodes$ctyid}.tif")))

# Filter out the completed ones.
fipscodes <- fipscodes %>%filter(!tif_exists)
# Now run again starting at the line where fips_list is created, which will rerun the counties that didn't complete.
