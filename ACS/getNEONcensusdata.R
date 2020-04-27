# loading libraries 
library(tidycensus)
library(tidyverse)

#######################################################
# Retrieve fips codes 
#######################################################
data_dir <- "/nfs/public-data/NEON_workshop_data/NEON"
NEONgeoids <-  readr::read_csv(file.path(data_dir, "NEON-AOP-CensusGEOIDs.csv"), col_types = "cccccccc")
state_use <- unique(NEONgeoids$STATEFP)

#######################################################
# Retrieve ACS dataset names and codes
#######################################################
# codes for ACS variables found here
# https://www.socialexplorer.com/data/ACS2017_5yr/metadata/?ds=ACS17_5yr
# to retrieve data, codes much be formatted as xxxx_001, xxxx_002, etc  
# survey = "acs5" is default argument for this dataset, gives 5 year estimate
ACScodes <- readr::read_csv(file.path(data_dir, "NEON-AOP-ACSdatasets.csv"), col_types = "cc")
ACScodes <- ACScodes %>% 
  mutate(dataset_sep = paste0(substr(dataset,1,6),"_",substr(dataset,7,9)))

########### Downloading Census Data ######################
# must set up .Renviron file with an API key requested from here:
# https://api.census.gov/data/key_signup.html
# once they send you a key, put it in the .Renviron file using function
# census_api_key('YOUR KEY', install = TRUE)

readRenviron("~/.Renviron") # gets your R environment
Sys.getenv("CENSUS_API_KEY") # displays your API key to double check it's available

# This gets data for state_use, a vector of all unique states with a NEON site.

ACSdataset <- unique(ACScodes$dataset_sep)  # vector of variable IDs from ACS

Get_Dataset <- function(acs_vars, states, ...){
  
  df <- get_acs(geography = "tract", variables = acs_vars,
                state = states, geometry = FALSE, survey = "acs5")
  
  NEON_ACS_j <- left_join(df, ACScodes[ ,2:3], by = c("variable" = "dataset_sep"))
  
  NEON_ACS_Geoid <- left_join(NEON_ACS_j, NEONgeoids, by = "GEOID") %>% 
    drop_na(Site)  # drops any rows without a NEON Site
  
  return(NEON_ACS_Geoid)
}

NEON_ACS <- Get_Dataset(ACSdataset, state_use)

# look at how many variables were pulled in for each track (44 variables requested)
# View(count(NEON_ACS, NAME.x))

write_csv(NEON_ACS, file.path(data_dir, "NEON_AOP_ACS.csv"))

