# Make CSV of county names and fips codes for the new wildfire figure and for the flood figure

library(tidycensus)
library(dplyr)

wf_co <- c(".Clark County, Idaho", ".Washington County, Colorado", 
  ".Daggett County, Utah", ".Glacier County, Montana", ".Grand County, Colorado", 
  ".Pershing County, Nevada", ".Elmore County, Idaho", ".Torrance County, New Mexico", 
  ".Duchesne County, Utah", ".Bonner County, Idaho", ".Cascade County, Montana", 
  ".Chelan County, Washington", ".Maricopa County, Arizona", ".Ada County, Idaho", 
  ".Pueblo County, Colorado")

wf_split <- gsub('.', '', wf_co, fixed = TRUE) %>%
  strsplit(',')

wf_co_df <- data.frame(county_name = unlist(lapply(wf_split, function(x) x[[1]])),
                       state_name = trimws(unlist(lapply(wf_split, function(x) x[[2]]))))

wf_co_df <- left_join(wf_co_df, fips_codes, by = c(state_name = 'state_name', county_name = 'county'))

flood_co <- read.csv('~/group_repos/SESYNC-NEON-DATA/DASY/risk_figure/counties_flooding.csv')
flood_co$fips <- unlist(lapply(regmatches(flood_co$Filename, gregexpr('[0-9]+', flood_co$Filename)), function(x) x[1]))

flood_co_df <- flood_co %>%
  filter(!is.na(fips)) %>%
  mutate(state_code = substr(fips, 1, 2),
         county_code = substr(fips, 3, 5)) %>%
  rename(state_name = State, county_name = County)

readr::write_csv(wf_co_df, '/nfs/qread-data/DASY/counties_wildfire.csv')
readr::write_csv(flood_co_df, '/nfs/qread-data/DASY/counties_flood.csv')
