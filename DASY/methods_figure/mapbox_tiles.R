# Get imagery with Mapbox

library(qdrmapbox)
set_mapbox_api_key('~/mapboxapikey.txt')

# Add a buffer around the box so that it will 
#x = c(-76.58, -76.43), y = c(38.97, 39.03))
zoom <- 12
upper_left <- c(39.03, -76.58) + c(.01, -.01)
lower_right <- c(38.95, -76.43) + c(-.01, .01)

tile_index <- find_tile_numbers(zoom = zoom, upper_left = upper_left, lower_right = lower_right)
download_dir = '/nfs/qread-data/DASY/mapbox_tiles'

tile_df <- download_mapbox_tiles(tile_numbers_mat = tile_index, download_dir = download_dir, resolution = 'high', jpg_quality = 90)

georeference_all_tiles(tile_df)

build_virtual_raster(tile_df, file.path(download_dir, 'annapimage.vrt'))

annapimage_raster <- stack(file.path(download_dir, 'annapimage.vrt'))
