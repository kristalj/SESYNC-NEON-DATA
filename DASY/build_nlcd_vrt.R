# Build virtual raster (VRT) from the NLCD impervious raster in IMG format
# by QDR 12 Nov 2020

library(gdalUtils)

direc <- "/nfs/rswanwick-data/DASY"
raster_file <- "NLCD_2016_Impervious_descriptor_L48_20190405.img"

# Run gdalbuildvrt with all defaults to just create a VRT that points to the IMG without any modification
gdalbuildvrt(gdalfile = file.path(direc, raster_file), output.vrt = file.path(direc, 'NLCD_2016_impervious.vrt'))

# Test whether this can be read using the raster package
test <- raster::raster(file.path(direc, 'NLCD_2016_impervious.vrt')) # Yep it works!
