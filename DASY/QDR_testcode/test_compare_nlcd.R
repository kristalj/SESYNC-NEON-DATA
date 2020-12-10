# Compare "my" NLCD impervious with feddata NLCD impervious

feddata_nlcd <- FedData::get_nlcd(template = pop.projected, label = 'AA County', year = 2016, dataset = "Impervious")
my_nlcd <- raster("/nfs/rswanwick-data/DASY/temp_files/countynlcd-24-003.tif")

pop.merc <- st_transform(pop, proj4string(feddata_nlcd))

feddata_nlcd <- crop(feddata_nlcd, pop.merc)
my_nlcd <- crop(my_nlcd, pop.projected)
feddata_nlcd <- mask(feddata_nlcd, pop.merc)
my_nlcd <- mask(my_nlcd, pop.projected)

feddata_nlcd[feddata_nlcd <= 1] <- NA
my_nlcd[my_nlcd <= 1] <- NA

annap <- SpatialPoints(coords = cbind(-76.49, 38.98), proj4string = CRS("+init=epsg:4326"))
set.seed(410)
annap <- SpatialPoints(coords = cbind(runif(100, -76.49, -76.48), runif(10, 38.98, 38.99)), proj4string = CRS("+init=epsg:4326"))
annap_mercator <- spTransform(annap, proj4string(feddata_nlcd))
annap_albers <- spTransform(annap, proj4string(my_nlcd))
(fed_annap <- extract(feddata_nlcd, annap_mercator))
(my_annap <- extract(my_nlcd, annap_albers))

annap_box <- as(raster::extent(-76.50, -76.48, 38.97, 38.99), "SpatialPolygons")
proj4string(annap_box) <- "+init=epsg:4326"
annapbox_mercator <- spTransform(annap_box, proj4string(feddata_nlcd))
annapbox_albers <- spTransform(annap_box, proj4string(my_nlcd))
(fed_annapbox <- extract(feddata_nlcd, annapbox_mercator))
(my_annapbox <- extract(my_nlcd, annapbox_albers))

fed_crop <- crop(feddata_nlcd, annapbox_mercator)
my_crop <- crop(my_nlcd, annapbox_albers)

par(mfrow=c(1,2))
plot(my_crop)
plot(fed_crop)


fivenum(fed_annapbox[[1]])
fivenum(my_annapbox[[1]])

my_annapbox_withNA <- my_annapbox
my_annapbox_withNA[[1]][my_annapbox_withNA[[1]] == 0] <- NA
fivenum(my_annapbox_withNA[[1]])
fivenum(fed_annapbox[[1]])

fed_values <- fed_annapbox[[1]][!is.na(fed_annapbox[[1]])]
my_values <- my_annapbox[[1]][my_annapbox[[1]] > 0]
