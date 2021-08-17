# Correct the nodata value in the tifs
cd /nfs/qread-data/DASY/tifs
outpath='/nfs/qread-data/temp/mdtifs'
mkdir $outpath
for tiffile in neon-dasy-24*.tif; do
  gdalwarp -dstnodata -9999 -co COMPRESS=LZW $tiffile $outpath/$tiffile
done

# Merge the corrected ones
cd $outpath
gdal_merge.py -o mdmerged.tif -of GTiff -co COMPRESS=LZW -co BIGTIFF=YES neon-dasy-*.tif

# Merge the raw ones
gdal_merge.py -o mdmergedraw.tif -of GTiff -co COMPRESS=LZW -co BIGTIFF=YES /nfs/qread-data/DASY/tifs/neon-dasy-24*.tif


### all
for tiffile in /nfs/qread-data/DASY/tifs/neon-dasy-*.tif; do
  gdalwarp -dstnodata -9999 -co COMPRESS=LZW $tiffile /nfs/qread-data/DASY/tifs_corrected/$tiffile
done