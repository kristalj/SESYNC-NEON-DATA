# Read flooding raster 

cstdepth_aa <- read_stars('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/CstDpth01pct.tif')
wse_aa <- read_stars('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/WSE_01pct.tif', proxy = FALSE)
wse_aa <- read_stars('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/WSE_01pct.tif', RAT = "ID")

library(raster)
wse_raster <- raster('/nfs/qread-data/DASY/frd/FRD_24003C_Coastal_GeoTIFFs_20150909/WSE_01pct.tif')

wse_raster_notna <- !is.na(wse_raster) %>% st_as_stars
wse_raster_notna_poly <- st_as_sf(wse_raster_notna, as_points = FALSE, merge = TRUE)

wse_raster_notna_poly_ourdasycrs <- st_transform(wse_raster_notna_poly, crs = st_crs(ourdasy_aa))
wse_raster_notna_poly_epadasycrs <- st_transform(wse_raster_notna_poly, crs = st_crs(epadasy_aa))
wse_raster_notna_poly_fbpopcrs <- st_transform(wse_raster_notna_poly, crs = st_crs(fbpop_aa))

wse_raster_sums <- aggregate(ourdasy_aa, wse_raster_notna_poly_ourdasycrs, FUN = sum, na.rm = TRUE)

grandtotal_classes <- function(env_poly, pop_raster) {
  data.frame(env_class = env_poly[[1]], pop = pop_raster[[1]]) %>%
    group_by(env_class) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
}

wse_raster_grandtotals_ourdasy <- grandtotal_classes(wse_raster_notna_poly_ourdasycrs, wse_raster_sums)