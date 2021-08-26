cd /nfs/qread-data/DASY/tifs
gdalbuildvrt dasy_conus_2021-08-26.vrt *.tif
gdal_translate -co COMPRESS=LZW -co BIGTIFF=YES dasy_conus_2021-08-26.vrt dasy_conus_2021-08-26.tif
