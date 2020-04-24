library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(glue)
library(fs)

data_dir <- "/nfs/public-data/NEON_workshop_data/NEON"

# read in AOP flight boxes shapefile
aop <- sf::st_read(file.path(data_dir, "NEON-AOP-FlightBoxes"))

# download shapefiles for all states 
# use cb = TRUE for less detailed version
us_states <- tigris::states(class = "sf")

state_codes <- us_states %>% 
  st_drop_geometry() %>%
  dplyr::select(STATEFP, STUSPS)

# spatial join of aop and states data
# in states projection (NAD83)
aop_join_states <- aop %>% 
  dplyr::select(-Name) %>%
  st_transform(crs = st_crs(us_states)) %>%
  st_join(us_states) %>% 
  dplyr::select(Site, STUSPS, geometry)

aop_sites <- aop$Site %>% unique() %>% as.character()

aop_site <- aop_sites[2]

##################################
## function based on one aop site
# uses objects: aop_join_states, state_codes
##################################

save_tracts_for_aop_site <- function(aop_site){
  # combined geometry of all flight boxes for site
  aop_site_sf <- dplyr::filter(aop_join_states, Site == aop_site) %>%
    st_union(by_feature = FALSE) %>% st_as_sf()
  
  # get all states that site includes
  aop_site_states <- dplyr::filter(aop_join_states, Site == aop_site) %>%
    dplyr::pull(STUSPS) %>% unique()
  
  # get all tracts for those states 
  aop_site_statetracts_list <- tryCatch({
    purrr::map(aop_site_states, ~tigris::tracts(state = .x, 
                                                class = "sf", 
                                                year = 2017,
                                                refresh = FALSE))
  }, error = function(e){
    purrr::map(aop_site_states, ~tigris::tracts(state = .x, 
                                                class = "sf", 
                                                year = 2018, # problem with NC 2017 (default year)
                                                refresh = FALSE))
  })
  # combine list of all tracts into one sf
  aop_site_statetracts_sf <- aop_site_statetracts_list %>% 
    purrr::map(~st_cast(.x, to = "MULTIPOLYGON")) %>%
    data.table::rbindlist() %>% st_as_sf()

  # aggregate tracts to counties
  aop_site_counties_sf <- aop_site_statetracts_sf %>%
    group_by(STATEFP, COUNTYFP) %>%
    dplyr::summarise(aland = sum(ALAND),
                     awater = sum(AWATER))
  
  # identify counties that overlap aop site
  mat1 <- aop_site_counties_sf %>%
    sf::st_intersects(aop_site_sf, spare = FALSE)
  ctys_in_aop <- which(apply(mat1, 1, any))
  ctys_subset <- aop_site_counties_sf[ctys_in_aop,]
  
  # calculate how much area of footprint in each county
  aop_x_cty <- aop_site_sf %>% st_intersection(ctys_subset)
  
  cty_data <- aop_x_cty %>% 
    mutate(cty_area_in_aop_m2 = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    mutate(aop_area_m2 = as.numeric(st_area(aop_site_sf)),
           prop_aop_in_cty = cty_area_in_aop_m2/aop_area_m2) %>%
    mutate(prop_ctyland_in_aop = cty_area_in_aop_m2/aland,
           prop_ctylandwater_in_aop = cty_area_in_aop_m2/(aland + awater)) %>%
    mutate(site_code = aop_site) %>%
    left_join(state_codes) %>%
    dplyr::select(site_code, STUSPS, STATEFP, COUNTYFP, aland:prop_ctylandwater_in_aop)

  cty_data %>% 
    readr::write_csv(path = glue::glue("data/census-counties/county-data-{aop_site}.csv"))
  
  # plot just counties that overlap aop
  county_map <- aop_site_statetracts_sf %>% 
    dplyr::filter(COUNTYFP %in% aop_x_cty$COUNTYFP) %>%
    ggplot() +
    geom_sf(aes(fill = COUNTYFP)) +
    geom_sf(data = aop_site_sf, col = "green", fill = NA) +
    theme(legend.position = "none")
  
  filename <- glue::glue("plots/census-counties/county-map-{aop_site}.pdf")
  pdf(filename)
  print(county_map)
  dev.off()

  # identify tracts that overlap aop site
  mat <- aop_site_statetracts_sf %>% 
    sf::st_intersects(aop_site_sf, sparse = FALSE)
  tracts_in_aop <- which(apply(mat, 1, any))
  tracts_subset <- aop_site_statetracts_sf[tracts_in_aop,]
  
  tracts_subset <- tracts_subset %>% left_join(state_codes)
  
  tracts_data <- tracts_subset %>% 
    st_drop_geometry() %>%
    dplyr::select(STUSPS, STATEFP, COUNTYFP, TRACTCE, GEOID, NAME, NAMELSAD) %>%
    mutate(Site = aop_site)
  
  tracts_data %>% 
    readr::write_csv(path = glue::glue("data/census-geoids/tracts-data-{aop_site}.csv"))
  
  g1 <- ggplot(tracts_subset) +
    geom_sf(aes(fill = STUSPS)) +
    geom_sf(data = aop_site_sf, col = "black", lwd = 1, fill = NA) +
    geom_sf_label(data = aop_site_sf, label = aop_site) +
    xlab(element_blank()) + ylab(element_blank()) +
    theme_bw()
  
  filename <- glue::glue("plots/census-geoids/tracts-map-{aop_site}.pdf")
  pdf(filename)
  print(g1)
  dev.off()
  
}

# try one
# save_tracts_for_aop_site(aop_site = aop_sites[1])
# run function over all sites
purrr::walk(aop_sites, ~save_tracts_for_aop_site(.x))

# combine data into one table and save as csv
fs::dir_ls("data/census-counties") %>%
  purrr::map_df(~readr::read_csv(.x, col_types = "ccccddddddd")) %>%
  readr::write_csv("NEON-AOP-CountyAreas.csv")

fs::dir_ls("data/census-geoids") %>%
  purrr::map_df(~readr::read_csv(.x, col_types = "cccccccc")) %>%
  readr::write_csv(file.path(data_dir, "NEON-AOP-CensusGEOIDs.csv"))
