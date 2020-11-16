# Shell script to download all data for the USA

# 1. Download and unzip all block group geographies.
# Get 2016 BG to match the existing script.
# A separate file is downloaded for each state.
cd /nfs/public-data/census-tiger-2016/BG
wget -r -np zip ftp://ftp2.census.gov/geo/tiger/TIGER2016/BG/
for i in *.zip; do unzip $i; done

# 2. Download and unzip all block geographies.
# 2010, to match the existing script.
# A separate file is downloaded for each state.
cd /nfs/public-data/census-tiger-2010/block
wget -r -np zip ftp://ftp2.census.gov/geo/tiger/TIGER2010BLKPOPHU/

# 2. Download and unzip NLCD 2016 imperviousness
cd /nfs/public-data/NLCD
wget https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2016_Impervious_L48_20190405.zip
mkdir NLCD_2016_Impervious_L48_20190405
mv NLCD_2016_Impervious_L48_20190405.zip NLCD_2016_Impervious_L48_20190405
cd NLCD_2016_Impervious_L48_20190405
unzip NLCD_2016_Impervious_L48_20190405.zip
