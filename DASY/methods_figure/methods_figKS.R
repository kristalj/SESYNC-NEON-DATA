# Methods figure with intermediate steps

# 1. Block group populations
# 2. Superimpose impervious surface on this
# 3. Remove roads from impervious surface
# 4. Final dasymetric map

library(tidycensus)
library(tidyverse)
library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(stars)

### State and county ID used for the example (can be changed)
stid <- '20'
ctyid <- '161'
ctyid2 <- '149'
# Part of this region is in Pottawatomie County, 149.

# Produce data for map (adapted from get_dasy_data.R) ---------------------

# Albers equal-area projection
aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

census_api_key(readLines('/research-home/qread/censusapikey.txt')) 
# This is done to not have the API key in your environment or scripts (good practice)

pop1 <- get_acs(geography = "block group", variables = "B01003_001", 
               year = 2016, state= stid, county = ctyid, 
               geometry = TRUE)   
pop2 <- get_acs(geography = "block group", variables = "B01003_001", 
                year = 2016, state= stid, county = ctyid2, 
                geometry = TRUE)   

pop <- rbind(pop1, pop2)

# Data QC: remove empty geometries from pop
pop <- pop[!is.na(st_dimension(pop)), ]

# Project population to Albers equal-area
pop.projected <- st_transform(pop, crs = aea)

# Update: use gdalwarp to extract the county area, from the NLCD impervious raster, already in Albers projection
# Use a temporary directory I created for the purpose to write the county polygon for extraction. 
nlcd_imp_vrt <- '/nfs/public-data/NLCD/VRTs/NLCD_2016_Impervious_L48_20190405.vrt'
temp_polygon_filename <- as.character(glue("/nfs/qread-data/temp/county-{stid}-{ctyid}.gpkg"))
temp_nlcdraster_filename <- as.character(glue("/nfs/qread-data/temp/countynlcd-{stid}-{ctyid}.tif"))
st_write(st_union(pop.projected), dsn = temp_polygon_filename, driver = 'GPKG', append = FALSE)
gdalwarp(srcfile = nlcd_imp_vrt, dstfile = temp_nlcdraster_filename, cutline = temp_polygon_filename, crop_to_cutline = TRUE, tr = c(30, 30), dstnodata = "None")
lu <- raster(temp_nlcdraster_filename)

#download 2010 block-level data, filter for only the blocks with 0 pop
zero.pop <- get_decennial(geography = "block", variables = "P001001", 
                          year = 2010, state = stid, county = ctyid, 
                          geometry = TRUE) %>% filter(value == 0) %>% st_transform(., aea)
zero.pop2 <- get_decennial(geography = "block", variables = "P001001", 
                          year = 2010, state = stid, county = ctyid2, 
                          geometry = TRUE) %>% filter(value == 0) %>% st_transform(., aea)

zero.pop <- rbind(zero.pop, zero.pop2)

#Remove NLCD data <=1% (masking no longer necessary as it's already masked)
lu[lu <= 1] <- NA

#create lu ratio
lu.ratio <- lu/100

#mask out zero pop blocks
lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse=TRUE)

#get the impervious surface descriptor dataset from: https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=category%3AUrban%20Imperviousness&f%5B2%5D=year%3A2016
# Now VRT is used.
imp.surf.desc <- raster("/nfs/public-data/NLCD/NLCD_2016_Impervious_descriptor_L48_20190405/NLCD_2016_Impervious_descriptor_L48_20190405.img")
#mask out primary, secondary, and urban tertiary roads
imp.surf.crop <- raster::crop(imp.surf.desc, as(pop.projected, "Spatial")) #crop imp surface to county
#plot(imp.surf.crop)
imp.surf.mask <- raster::mask(imp.surf.crop, as(pop.projected, "Spatial")) #mask all non-county values to NA

reclass.table <- matrix(c(1,6,1,7,14,NA), ncol = 3, byrow = TRUE) #reclassify values 1-6 into 1 for keep drop the rest

imp.roads <- reclassify(imp.surf.mask, reclass.table, right = NA)
imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp, method = 'ngb')#have to reproject the descriptor file
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



# Create maps -------------------------------------------------------------

# Calculate bounding box in Albers coordinates 
plot_box <- st_as_sf(data.frame(x = c(-96.65, -96.45), y = c(39.11, 39.21)), coords = c('x', 'y'), crs = 4326) %>%
  st_transform(crs = aea) %>%
  st_bbox

# Reclassify road versus not road
reclass.table2 <- matrix(c(1,6,1,7,14,2), ncol = 3, byrow = TRUE) #reclassify values 1-6 into 1 for keep drop the rest

imp.roads2 <- reclassify(imp.surf.mask, reclass.table2, right = NA)
imp.roads.p2 <- projectRaster(imp.roads2, lu.ratio.zp, method = 'ngb')#have to reproject the descriptor file

lu_stars <- st_as_stars(lu)
imp_stars <- st_as_stars(imp.roads.p2)
imp_stars[[1]] <- as.character(imp_stars[[1]])
imp_stars[imp_stars == 0] <- NA
dasy_stars <- st_as_stars(dasy.pop)


# Create maps with mapbox tiles -------------------------------------------

library(raster)
library(ggspatial)
library(qdrmapbox)
library(gridExtra)

download_dir = '/nfs/qread-data/DASY/mapbox_tiles_ks'
ksimage_raster <- stack(file.path(download_dir, 'ksimage.vrt'))

maptheme <- theme(legend.position = 'bottom', legend.text = element_text(size = rel(0.5)), legend.title = element_text(size = rel(0.6)),
                  axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
mapcoord <- coord_sf(crs = aea, xlim = plot_box[c(1,3)], ylim = plot_box[c(2,4)], expand = FALSE)

image_alpha <- 0.8

p1map <- ggplot() +
  annotation_spatial(data = ksimage_raster, alpha = image_alpha) +
  geom_sf(data = pop.projected, color = 'white', alpha = 0.3, aes(fill = estimate)) +
  scale_fill_viridis_c(name = 'block group\npopulation', option = 'B') +
  mapcoord + maptheme

p2map <- ggplot() +
  annotation_spatial(data = ksimage_raster, alpha = image_alpha) +
  geom_stars(data = lu_stars/100) +
  geom_sf(data = pop.projected, color = 'white', alpha = 0.3, fill = NA, size = 0.3) +
  scale_fill_viridis_c(name = 'impervious surface\npercentage', labels = scales::percent, na.value = 'transparent', option = 'B') +
  mapcoord + maptheme

p3map <- ggplot() +
  annotation_spatial(data = ksimage_raster, alpha = image_alpha) +
  geom_stars(data = imp_stars) +
  geom_sf(data = pop.projected, color = 'white', alpha = 0.3, fill = NA, size = 0.3) +
  scale_fill_brewer(name = 'surface type', palette = 'Set2', labels = c('road (discarded)', 'non-road (kept)'), na.value = 'transparent', na.translate = FALSE) +
  mapcoord + maptheme 

p4map <- ggplot() +
  annotation_spatial(data = ksimage_raster, alpha = image_alpha) +
  geom_stars(data = dasy_stars) +
  geom_sf(data = pop.projected, color = 'white', alpha = 0.3, fill = NA, size = 0.3) +
  scale_fill_viridis_c(name = 'dasymetric\npopulation', na.value = 'transparent', option = 'B') +
  mapcoord + maptheme

# Bind together so that main panels line up.
allmaps <- gtable_cbind(ggplotGrob(p1map), ggplotGrob(p2map), ggplotGrob(p3map), ggplotGrob(p4map))
ggsave('/nfs/qread-data/DASY/figs/methods_figure_draft_kansas.png', allmaps, height = 4, width = 12, dpi = 300)

# FIXME Add MapBox logo and credits somewhere on this map.

  # mapbox_logo(xmin = -8870000, xmax = -8820000, ymin = 4535000, ymax = 4590000) +
  # annotate('text', x = Inf, y = -Inf, label = '\u00a9 Mapbox \u00a9 OpenStreetMap', hjust = 1, vjust = -1, color = 'white', size = 2) 
