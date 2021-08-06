library(tidyverse)

load('~/Documents/temp/dasy/grandtotals_finalfig.RData')

# Get proportions

label_names <- c('This study', 'U.S. EPA', 'Huang et al.', 'Facebook', 'Block group area weighting')
estimate_labels <- data.frame(estimate = c('our_dasy', 'epa_dasy', 'huang', 'fb', 'blockgroup'),
                              estimate_label = factor(label_names, levels = label_names))

# Reshape so we have a population total column and a proportion column for each estimation method
wildfire_risks_long <- wildfire_grandtotals %>%
  select(county_label, fips, env_class, our_dasy:blockgroup) %>%
  pivot_longer(our_dasy:blockgroup, names_to = 'estimate', values_to = 'population') %>%
  group_by(county_label, fips, estimate) %>%
  mutate(proportion = population/sum(population, na.rm = TRUE)) %>%
  left_join(estimate_labels)

flood_risks_long <- flood_grandtotals %>%
  select(county_label, fips, env_class, our_dasy:blockgroup) %>%
  pivot_longer(our_dasy:blockgroup, names_to = 'estimate', values_to = 'population') %>%
  group_by(county_label, fips, estimate) %>%
  mutate(proportion = population/sum(population, na.rm = TRUE)) %>%
  left_join(estimate_labels)

# Reduce data to a single risk category and select five counties for each risk type.
counties_use_wf <- c('49009', '35057', '49013', '53007', '16001')
wildfire_risks_reduced <- wildfire_risks_long %>%
  filter(env_class %in% as.character(3:5), fips %in% counties_use_wf) %>%
  group_by(county_label, estimate, estimate_label) %>%
  summarize(proportion = sum(proportion), population = sum(population))

counties_use_fl <- c('24019', '13029', '12035', '01003', '24003')
flood_risks_reduced <- flood_risks_long %>%
  filter(env_class %in% '1', fips %in% counties_use_fl) 

ggplot(flood_risks_reduced, aes(x = county_label, y = proportion, color = estimate_label)) +
  geom_point(size = 3, alpha = 0.5, position = position_dodge(width = 0.05)) +
  scale_y_continuous(name = 'Proportion of population at risk from flooding',
                     limits = c(0, max(flood_risks_reduced$proportion)),
                     expand = expansion(mult = c(0, 0.02)), labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  scale_color_brewer(name = 'Dasymetric method', palette = 'Dark2') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank())

ggplot(wildfire_risks_reduced, aes(x = county_label, y = proportion, color = estimate_label)) +
  geom_point(size = 3, alpha = 0.5, position = position_dodge(width = 0.05)) +
  scale_y_continuous(name = 'Proportion of population at risk from wildfire',
                     limits = c(0, max(wildfire_risks_reduced$proportion)),
                     expand = expansion(mult = c(0, 0.02)), labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  scale_color_brewer(name = 'Dasymetric method', palette = 'Dark2') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank())


# Alternative with logscale -----------------------------------------------

# Relevel and swap orders. Just do manually

okabe_colors <- palette.colors(7, palette = 'Okabe-Ito')[c(7, 3, 4, 2, 1)]

p_flood <- flood_risks_reduced %>%
  ungroup %>%
  mutate(county_label = factor(county_label, labels = c('Dorchester, Maryland\n(32,500)', 'Bryan, Georgia\n(34,100)', 'Flagler, Florida\n(103,000)', 'Baldwin, Alabama\n(200,000)', 'Anne Arundel,Maryland\n(560,000)')) %>% fct_rev,
         percentage = paste0(round(proportion * 100, 1), '%')) %>%
  ggplot(aes(x = county_label, y = population, color = estimate_label)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_y_log10(name = 'Population at risk from flooding',
                limits = c(110, 44000),
                expand = expansion(mult = 0.02), position = 'right') +
  coord_flip() +
  scale_color_manual(name = 'Dasymetric method', values = unname(okabe_colors)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(1, 0.2, 0.1, 2), 'cm'))

p_wildfire <- wildfire_risks_reduced %>%
  ungroup %>%
  mutate(county_label = factor(county_label, labels = c('Daggett, Utah\n(750)', '  Torrance, New Mexico\n(15,600)', 'Duchesne, Utah\n(20,100)', 'Chelan, Washington\n(74,800)', 'Ada, Idaho\n(426,000)')) %>% fct_rev) %>%
ggplot(aes(x = county_label, y = population, color = estimate_label)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_y_log10(name = 'Population at risk from wildfire',
                limits = c(110, 44000),
                expand = expansion(mult = 0.02)) +
  coord_flip() +
  scale_color_manual(name = 'Dasymetric method', values = unname(okabe_colors)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.2, 0.25),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.5)),
        legend.key.size = unit(0.34, 'cm'),
        legend.margin = margin(0, 0.1, 0.1, 0.1, unit = 'cm'),
        legend.background = element_rect(fill = 'white', colour = 'gray50', size = 0.5), 
        plot.margin = unit(c(0.1, 0.2, 1, 2), 'cm'))

# Inset maps --------------------------------------------------------------

library(USAboundaries)
library(sf)
library(cowplot)

countymaps <- us_counties(resolution = 'high') %>%
  st_transform(3857) %>%
  mutate(fips = paste0(statefp, countyfp))

make_inset <- function(cofips) {
  statefips <- substr(cofips, 1, 2)
  state <- countymaps %>% filter(statefp == statefips)
  county <- state %>% filter(fips == cofips)
  p <- ggplot() + 
    geom_sf(data = st_geometry(state), fill = NA, size = 0.3) + 
    geom_sf(data = st_geometry(county), fill = 'red', color = NA) +
    theme_void()
  p
}

# Loop through and create the maps
insets_flood <- map(counties_use_fl, make_inset)
insets_wildfire <- map(counties_use_wf, make_inset)


inset_size <- 0.12
inset_x <- 0.03
p_top <- ggdraw(p_flood) +
  draw_plot(insets_flood[[1]], x = inset_x, y = 0.6, width = 0.1, height = 0.1) +
  draw_plot(insets_flood[[2]], x = inset_x, y = 0.45, width = inset_size, height = inset_size) +
  draw_plot(insets_flood[[3]], x = inset_x, y = 0.3, width = inset_size, height = inset_size) +
  draw_plot(insets_flood[[4]], x = inset_x, y = 0.16, width = inset_size, height = inset_size) +
  draw_plot(insets_flood[[5]], x = inset_x, y = 0.03, width = 0.1, height = 0.1)
p_bottom <- ggdraw(p_wildfire) +
  draw_plot(insets_wildfire[[1]], x = inset_x, y = 0.85, width = inset_size, height = inset_size) +
  draw_plot(insets_wildfire[[2]], x = inset_x, y = 0.7, width = inset_size, height = inset_size) +
  draw_plot(insets_wildfire[[3]], x = inset_x, y = 0.55, width = inset_size, height = inset_size) +
  draw_plot(insets_wildfire[[4]], x = inset_x, y = 0.4, width = inset_size, height = inset_size) +
  draw_plot(insets_wildfire[[5]], x = inset_x, y = 0.27, width = inset_size, height = inset_size)

p_all <- plot_grid(p_top, p_bottom, nrow = 2)

ggsave('~/Documents/temp/dasy/logscale_pop_fig_with_insets.png', p_all, height = 6, width = 6, dpi = 400)
