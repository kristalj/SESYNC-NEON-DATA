library(dplyr)
library(sf)
library(mapview)
library(leaflet)

data_dir <- "/nfs/public-data/NEON_workshop_data/NEON" 
aop_kmz_filepath <- "https://www.neonscience.org/aop-flight-boundaries-kmz"
aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz")
#download.file(aop_kmz_filepath, destfile = aop_kmz_localfile)

### Will need to read in Hawaii KMLs as well, merge into single sf object
aop_kmz_H1 <- glue::glue("/research-home/rswanwick/neon-workshop-data/D20_PUUM_C1_P1_v3.kml")
aop_kmz_H2 <- glue::glue("/research-home/rswanwick/neon-workshop-data/D20_PUUM_C1_P2_v3.kml")

H1 <- st_read(aop_kmz_H1)
H2 <- st_read(aop_kmz_H2)

H_comb <- rbind(H1,H2) %>% st_zm() %>% 
  mutate(Name=c("D20_PUUM_C1_P1_v3","D20_PUUM_C1_P2_v3")) %>%
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  group_by(Site) %>%
  summarise_all(first) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(Description="")
# H1 and H2 are already multipolygons, but adding this line
# conveniently re-arranges the columns into the same order as aop_sf below

aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz") 
#aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz")
aop_sf <- st_read(aop_kmz_localfile) %>% st_zm() %>% 
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  group_by(Site) %>%
  summarise_all(first) %>%
  st_cast("MULTIPOLYGON") ## Included these last few lines to merge the polygons based on site
## This makes things easier and accounts for co-incidemt terrestrial and aquatic sites polygons

#specify the columns in order to the tables to merge 
aop_sf <- rbind(aop_sf[,c(1,2,14,15)], H_comb[,c(1,2,14,15)])

#how many sites 
sitecodes <- unique(aop_sf$Site)
length(sitecodes)

#Create leaflet map 
#Use this to view that polygons have merged correctly
#And that all are accounted for
aop_sf %>%
  leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addPolygons(popup = ~Name) %>%
  addLayersControl(baseGroups = c("Esri World Imagery", "Open Street Map"))

#taking images of the sites 
site_box1 <- aop_sf %>%
  filter(sitecodes == "D01_HARV") %>%
  st_bbox()

aop_sf %>%
  filter(sitecodes == "D01_HARV") %>%
  leaflet() %>%
  fitBounds(lng1 = site_box1[["xmin"]],
            lng2 = site_box1[["xmax"]],
            lat1 = site_box1[["ymin"]],
            lat2 = site_box1[["ymax"]]) %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addPolygons(popup = ~Name, fillOpacity = 0, opacity = 1,
              weight = 10, color = "yellow") %>%
  addLayersControl(baseGroups = c("Esri World Imagery", "Open Street Map"),
                   options = layersControlOptions(collapsed = FALSE))

#saving pdf of flight box 
save_neonmap <- function(my_sitename, my_vwidth = 12000, my_vheight = 8000){
  
  site_box1 <- aop_sf %>%
    filter(sitecodes == my_sitename) %>%
    st_bbox()
  
  mp <- aop_sf %>%
    filter(sitecodes == my_sitename) %>%
    leaflet() %>%
    fitBounds(lng1 = site_box1[["xmin"]],
              lng2 = site_box1[["xmax"]],
              lat1 = site_box1[["ymin"]],
              lat2 = site_box1[["ymax"]]) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
    addPolygons(popup = ~Name, fillOpacity = 0, opacity = 1,
                weight = 10, color = "yellow") 
  
  
  mapshot(mp, 
          file = glue::glue("map_site-{my_sitename}.pdf"), 
          vwidth = my_vwidth, vheight = my_vheight)
}


#use the function one at a time to save images 
save_neonmap(my_sitename = "D18_OKSR")

#save all neon sites images 
purrr::walk(sitecodes, ~save_neonmap(.x))
