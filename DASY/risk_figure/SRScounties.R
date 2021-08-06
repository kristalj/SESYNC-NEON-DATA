# Get a stratified random sample of coastal counties in the USA, by population
# https://www2.census.gov/library/stories/2018/08/coastline-counties-list.xlsx
# Take a bigger sample to ensure we get 2 counties with WSE GeoTIFFs from each quintile.
library(readxl)
library(dplyr)

coastal_counties <- read_xlsx('~/Documents/temp/coastline-counties-list.xlsx', skip = 3, .name_repair = 'universal') %>%
  filter(!is.na(V.2016..POPULATION..ESTIMATE))

# Split into quintiles and select 3 random counties from each quintile
set.seed(2000)
quintiles <- quantile(coastal_counties$V.2016..POPULATION..ESTIMATE, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
# First sample
c1 <- coastal_counties %>%
  mutate(quintile = cut(V.2016..POPULATION..ESTIMATE, breaks = quintiles, include.lowest = TRUE, right = FALSE)) %>%
  group_by(quintile) %>%
  sample_n(3)
# Second sample
c2 <- coastal_counties %>%
  mutate(quintile = cut(V.2016..POPULATION..ESTIMATE, breaks = quintiles, include.lowest = TRUE, right = FALSE)) %>%
  group_by(quintile) %>%
  sample_n(5)
# Third sample (continue sampling until we have 10 good ones)
c3 <- coastal_counties %>%
  mutate(quintile = cut(V.2016..POPULATION..ESTIMATE, breaks = quintiles, include.lowest = TRUE, right = FALSE)) %>%
  group_by(quintile) %>%
  sample_n(5)
c4 <- coastal_counties %>%
  mutate(quintile = cut(V.2016..POPULATION..ESTIMATE, breaks = quintiles, include.lowest = TRUE, right = FALSE)) %>%
  group_by(quintile) %>%
  sample_n(5)

# Stratified random sample of counties in western USA to use for wildfire risk, by population
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres.xlsx

usa_counties <- read_xlsx('~/Documents/temp/co-est2019-annres.xlsx', skip = 3, .name_repair = 'universal') %>%
  filter(!is.na(Census), !(...1 %in% 'United States'))

# Filter to only some western states
w_states <- c('California', 'Oregon', 'Washington', 'Idaho', 'Nevada', 'Arizona', 'Utah', 'Montana', 'Colorado', 'New Mexico', 'Wyoming')


west_counties <- usa_counties %>%
  mutate(state = trimws(purrr::map(strsplit(...1, ','), 2))) %>%
  filter(state %in% w_states)

set.seed(21401)
quintiles <- quantile(west_counties$Census, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
west_counties %>%
  mutate(quintile = cut(Census, breaks = quintiles, include.lowest = TRUE, right = FALSE)) %>%
  group_by(quintile) %>%
  sample_n(3)
