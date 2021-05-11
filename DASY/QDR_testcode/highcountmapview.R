# Where are the pixels with pop > 400 in AA county, according to Mark Zuckerberg

# Find the coordinates of the points and blow them up on a map.

fb_high_counts <- aafb %>%
  st_as_sf(as_points = TRUE) %>%
  filter(aa_pop_fb_longlat.tif > 400)

ggplot() +
  geom_sf(data = fb_high_counts) +
  geom_sf(data = st_geometry(aabounds_longlat), fill = NA)

library(mapview)

mapview(fb_high_counts) +
  mapview(aadasy, alpha = 0.5)

mapview(fb_high_counts) +
  mapview(aafb, alpha = 0.2)
