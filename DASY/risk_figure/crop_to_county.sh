tifpath="/nfs/qread-data/DASY/countyrasters"
gpkgpath="/nfs/qread-data/DASY/countybounds"
{
  read
  while IFS=, read -r county state fips; do 
    echo "Processing $county County, $state. FIPS $fips"; 
    
    statefips=`echo $fips | cut -c 1-2`
    countyfips=`echo $fips | cut -c 3-5`
    
    gdalwarp /nfs/qread-data/DASY/whp/Data/whp2020_GeoTIF/whp2020_cls_conus.tif ${tifpath}/county${fips}_wildfire.tif \
     -overwrite -crop_to_cutline -cutline ${gpkgpath}/county${fips}.gpkg
    gdalwarp /nfs/qread-data/DASY/epadasy/dasymetric_us_20160208/dasymetric_us_20160208.tif ${tifpath}/county${fips}_epadasy.tif \
     -overwrite -crop_to_cutline -cutline ${gpkgpath}/county${fips}.gpkg
  done 
} < /nfs/qread-data/DASY/countycodesforfb.csv