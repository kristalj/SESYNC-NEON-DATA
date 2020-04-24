# NLCD data for NEON AOP footprints
# devtools::install_github("ropensci/FedData")

library(sf)
library(FedData)
library(dplyr)
library(tidyr)
# library(ggplot2)
# library(data.table)
library(glue)
library(fs)
library(raster)

data_dir <- "/nfs/public-data/NEON_workshop_data/NEON"

# read in AOP flight boxes shapefile
aop <- sf::st_read(file.path(data_dir, "NEON-AOP-FlightBoxes")) %>% 
  dplyr::select(-Name) %>% 
  group_by(Site) %>% 
  summarise_all(first) %>%
  st_cast("MULTIPOLYGON")

aop_sites <- unique(aop$Site) %>% as.character()

aop_site <- aop_sites[55]
nlcd_codes <- readr::read_csv("nlcd_legend_2011.csv")
# https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend

download.file("https://www.neonscience.org/science-design/field-sites/export", 
              destfile = "neon-field-sites.csv")

neon_site_data <- readr::read_csv("neon-field-sites.csv") %>% 
  dplyr::select(`Domain Number`, `Site ID`, State) %>%
  mutate(Site = glue::glue("{`Domain Number`}_{`Site ID`}")) %>%
  mutate(landmass = dplyr::case_when(State == "PR" ~ "PR",
                                     State == "AK" ~ "AK",
                                     State == "HI" ~ "HI",
                                     !State %in% c("PR", "AK", "HI") ~ "L48")) %>%
  mutate(nlcd_year = dplyr::case_when(State == "PR" ~ 2001,
                                     State == "AK" ~ 2011,
                                     State == "HI" ~ 2001,
                                     !State %in% c("PR", "AK", "HI") ~ 2016)) %>%
  dplyr::select(Site, landmass, nlcd_year) %>% 
  mutate(Site = as.character(Site)) %>%
  add_row(Site = "D05_CHEQ", landmass = "L48", nlcd_year = 2016) %>%
  add_row(Site = "D18_BARO", landmass = "AK", nlcd_year = 2011)

aop_x_sitedata <- aop %>% left_join(neon_site_data)
  
aop_site <- aop_sites[1]
# for one site
get_nlcd_percents <- function(aop_site, dataset_type = "Land_Cover"){
  
  aop_site_sf <- aop_x_sitedata %>% dplyr::filter(Site == aop_site)
  site_landmass <- aop_site_sf %>% pull(landmass)
  nlcd_year <- aop_site_sf %>% pull(nlcd_year)
    
  nlcd_site <- FedData::get_nlcd(aop_site_sf,
                      label = aop_site,
                      dataset = dataset_type, 
                      landmass = site_landmass,
                      year = nlcd_year,
                      force.redo = FALSE)
  
  aop_site_sf_prj <- aop_site_sf %>% st_transform(proj4string(nlcd_site))
  nlcd_site_mask <- raster::mask(nlcd_site, as(aop_site_sf_prj, "Spatial"))
  
  if(dataset_type == "Land_Cover"){
    filename <- glue::glue("plots/nlcd/landcover-{aop_site}-{nlcd_year}.png")
    png(filename)
    # nlcd_agg <- raster::disaggregate(nlcd_site, fact = 3)
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "red")
    dev.off()    

  # tabulate number of cells in each type and 
  # Merge with legend to see land cover types
  cover <- raster::freq(nlcd_site_mask) %>%
    as.data.frame() %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(nlcd_codes, by = c("value" = "Class")) %>%
    dplyr::mutate(percent_cover = count/sum(count)) %>%
    dplyr::select(class_name, count, percent_cover) %>%
    mutate(Site = aop_site)
  
  }
  
  if(dataset_type == "Impervious"){
    hist_data <- hist(nlcd_site_mask, plot = FALSE,
         breaks = seq(0,100, 5))
    hist_df <- data.frame(site = aop_site,
               imperv_class = hist_data$breaks[2:21],
               imperv_freq = hist_data$counts,
               stringsAsFactors = FALSE)
    
    hist_df %>% readr::write_csv(glue::glue("data/nlcd/imperv_hist_data-{dataset_type}-{aop_site}-{nlcd_year}.csv"))
    
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    
    filename <- glue::glue("plots/nlcd/impervious-{aop_site}-{nlcd_year}.png")
    png(filename)
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "green")
    dev.off()    
    
    # area above 0%, 5%, 10%, 50% impervious

    filename <- glue::glue("plots/nlcd/impervious-hist-{aop_site}-{nlcd_year}.pdf")
    pdf(filename)
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    dev.off()
    
    rcl <- matrix(c(-Inf, 5, 100,
                    5, 10, 200, 
                    10, 50, 300,
                    50, 100, 400), ncol = 3, byrow = TRUE)
    imp_rcl <- nlcd_site_mask %>%
      raster::reclassify(rcl, include.lowest = TRUE)

    imp_val_df <- data.frame(value = c(100, 200, 300, 400),
                             class = c("below 5 percent", "5-10%", "10-50%", "over 50%"),
                             stringsAsFactors = FALSE)
    # tabulate area in each class
    cover <- raster::freq(imp_rcl) %>%
      as.data.frame() %>%
      dplyr::left_join(imp_val_df, by = "value") %>%
      dplyr::mutate(area_m2 = count*(30*30)) %>%
      mutate(Site = aop_site)
  }
    
  
  cover %>% readr::write_csv(glue::glue("data/nlcd/{dataset_type}-{aop_site}-{nlcd_year}.csv"))
  
}

aop_noD18 <- aop_sites[-c(49:54)]

get_nlcd_percents(aop_noD18[1], dataset_type = "Impervious")
get_nlcd_percents(aop_sites[55], dataset_type = "Land_Cover")

purrr::walk(aop_sites, ~get_nlcd_percents(.x, dataset_type = "Land_Cover"))
purrr::walk(aop_noD18, ~get_nlcd_percents(.x, dataset_type = "Impervious"))


# combine land cover data into one table and save as csv

all_aop_landcover <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  mutate(percent_cover = percent_cover*100) %>%
  tidyr::spread(key = class_name, value = percent_cover, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover.csv"))

all_aop_landcover_areas <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>% 
  mutate(area_ha = (count*30)/1e4) %>%
  dplyr::select(-count, -percent_cover) %>%
  tidyr::spread(key = class_name, value = area_ha, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover_areas %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover_areas.csv"))

# combine impervious data into one table and save as csv

all_aop_impervious <- fs::dir_ls("data/nlcd", regexp = "Impervious") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  dplyr::select(Site, area_m2, class) %>%
  tidyr::spread(key = class, value = area_m2,2, fill = 0) %>%
  dplyr::select(Site, 4, 3, 2, 5, 6)

all_aop_impervious %>% readr::write_csv(file.path(data_dir, "NEON-AOP-Impervioius.csv"))
