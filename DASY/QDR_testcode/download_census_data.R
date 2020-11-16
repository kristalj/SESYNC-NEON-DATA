# Download the ACS and decennial variables for the entire USA at once using tidycensus
# QDR 13 Nov 2020

library(tidycensus)

census_api_key(readLines('/nfs/rswanwick-data/rswanwick_census_api_key.txt')) 

# ACS: download B00001_001 for 2016 at block group level, without shapefile.
# For all block groups across all states!
all_states <- unique(fips_codes$state_code)
pop_acs_all <- get_acs(geography = 'block group',
                       variables = 'B00001_001',
                       state = all_states,
                       year = 2016,
                       geometry = FALSE
)

# Decennial: download P001001 for 2010 at block level, without shapefile.
