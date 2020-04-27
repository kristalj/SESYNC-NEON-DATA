# Mapping Neon sites with human impacts
#some change

library(leaflet)
library(sf)
library(readr)

data_dir <- "/nfs/public-data/NEON_workshop_data/NEON"


# KML FilES
# aquatic sites
aop_path_aq <- file.path(data_dir, "NEON_Aquatic_Site_Flight_Box_KMLs/")
aop_files_aq <- fs::dir_ls(aop_path_aq, regexp = ".kml$")

aop_files_aq_names <- basename(aop_files_aq) %>% substr(1, 8)

aop_aqbox_list <- purrr::map(aop_files_aq, ~st_read(.x)) %>% 
  purrr::map(~dplyr::select(.x, Name, geometry))
# aop_aqbox_list %>% purrr::map(~names(.x))

aop_aqbox_sf <- st_as_sf(data.table::rbindlist(aop_aqbox_list, idcol = "Site")) %>% 
  st_zm() %>%
  st_transform(32618) %>% st_transform(4326)

# terrestrial sites
# aquatic sites
aop_path_terr <- file.path(data_dir, "NEON_Terrestrial_Site_Flight_Box_KMLs/")
aop_files_terr <- fs::dir_ls(aop_path_terr, regexp = ".kml$")
aop_files_terr_names <- basename(aop_files_terr) %>% substr(1, 8)

aop_terrbox_list <- purrr::map(aop_files_terr, ~st_read(.x)) %>% 
  purrr::map(~dplyr::select(.x, Name, geometry))
names(aop_terrbox_list) <- aop_files_terr_names

aop_terrbox_sf <- st_as_sf(data.table::rbindlist(aop_terrbox_list, idcol = "Site")) %>% 
  st_zm() %>%
  st_transform(32618) %>% 
  st_transform(4326)

all_aops_sf <- rbind(aop_terrbox_sf, aop_aqbox_sf)
all_aops_centroids_sf <- all_aops_sf %>% st_centroid(all_aops_sf)


nlcd <- "https://smallscale.nationalmap.gov/arcgis/services/LandCover/MapServer/WMSServer"
wbd <- "https://hydro.nationalmap.gov/arcgis/services/wbd/MapServer/WMSServer"

# LEAFLET MAP
leaflet(neon_sites_sf) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addMarkers(data = all_aops_centroids_sf, group = "AOP centroids") %>%
  addWMSTiles(nlcd, layers = "1",
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              group = "NLCD") %>%
  addWMSTiles(wbd, layers = "7",
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              group = "WBD transparent") %>%
  addPolygons(data = neon_domains, fillOpacity = 0, opacity = 1, weight = 0.5,
              color = "white", group = "Domains") %>%
  addPolygons(data = aop_aqbox_sf, fillOpacity = 0, group = "Aquatic AOP",
              opacity = 1, color = "red") %>%
  addPolygons(data = aop_terrbox_sf, fillOpacity = 0,
              opacity = 1, color = "red", group = "Terrestrial AOP") %>%
  addCircleMarkers(data = neon_sites_sf, 
                   label = ~as.character(`Site Name`), 
                   radius = 3,
                   color = "yellow",
                   stroke = TRUE,
                   fillColor = "yellow",
                   opacity = 1, group = "Site Names") %>%
  addLayersControl(baseGroups = c("Esri World Imagery", "NLCD"),
                   overlayGroups = c("Site Names","WBD transparent",
                                     "Aquatic AOP", "Terrestrial AOP",
                                     "Domains",  "AOP centroids"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(group = c("WBD transparent", "AOP centroids"))
            
