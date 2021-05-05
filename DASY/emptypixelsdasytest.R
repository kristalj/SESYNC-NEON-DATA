library(sf)
library(stars)
library(gdalUtils)

# Read DASY map for AA county
aadasy <- read_stars('/nfs/rswanwick-data/DASY/tifs/neon-dasy-24-003.tif')

aadasy[[1]][is.na(aadasy[[1]])] <- -9999 # Replace NA with a numeric value so the mask will only mask out values outside the boundaries.

# Get AA boundaries
countybounds <- st_read('/nfs/qread-data/raw_data/landuse/USA/USA_county_2014_aea.gpkg')
aabounds <- countybounds %>%
  subset(fips == "24003") 

aabounds_dasyproj <- aabounds %>%
  st_transform(st_crs(aadasy))

# write AA boundaries to a file
st_write(aabounds, '~/temp/aacounty.gpkg', driver = 'GPKG', overwrite = TRUE)

aadasymask <- aadasy[aabounds_dasyproj] # Mask out values outside AA county boundaries

# Crop NLCD to AA county so we do not have to read all of it in to get its values.
gdalwarp('/nfs/public-data/NLCD/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img', '/research-home/qread/temp/aanlcd.tif', crop_to_cutline = TRUE, cutline = '/research-home/qread/temp/aacounty.gpkg')

# Read the AA county portion of NLCD in.
aanlcd <- read_stars('~/temp/aanlcd.tif')

# Crosstabulate all the pixels.
finaltable <- table(aanlcd[[1]] != 11, aadasymask[[1]] != -9999, useNA = "always")
finaltable_margins <- addmargins(finaltable)

# Show that 26.5% of pixels with land area inside county boundaries have data.
finaltable_margins['Sum', 1:2] / sum(finaltable_margins['Sum', 1:2])
