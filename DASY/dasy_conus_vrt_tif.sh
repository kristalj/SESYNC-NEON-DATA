cd /nfs/qread-data/DASY/tifs
gdalbuildvrt -vrtnodata -9999 dasy_conus_2021-08-12.vrt *.tif
gdal_translate -co COMPRESS=LZW -co BIGTIFF=YES dasy_conus_2021-08-12.vrt dasy_conus_2021-08-12.tif
