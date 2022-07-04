###############################
### Creating maps for paper ###
###############################

## Purpose: Create Figure X on mapping random effect for CIS geography.

# Libraries
library(patchwork)
library(geogrid)
library(ggplot2)
library(viridis)
library(tmap)
library(sf)

## Load all data ##

england <- read_sf(dsn = "./CIS geography/Covid_Infection_Survey__October_2020__EN_BGC.shp") # Load CIS Geography

# # Create hexmap
# par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
# for (i in 1:6) { # Run for different seeds and plot - select best
#   new_cells <- calculate_grid(shape = england, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
# 
# hexmap <- calculate_grid(shape = england, grid_type = "hexagonal", seed = 4) # Save solution
# resulthex <- assign_polygons(england, hexmap) # Assign polyons to new locations (slow)
# 
# # Save
# st_write(resulthex, "./CIS geography/cis_england_hexmap.shp", append = F)

resulthex <- read_sf(dsn = "./CIS geography/cis_england_hexmap.shp") # Load CIS hexmap
re_model <- read.csv("./20210304 MG Pub 2001021/re_effects_geo.csv") # Load RE estimates

# Join RE estimates onto shapefiles
england <- sp::merge(england, re_model, by.x = "CIS20CD", by.y = "grp", all.x=T) # Merge
resulthex <- sp::merge(resulthex, re_model, by.x = "CIS20CD", by.y = "grp", all.x=T) # Merge

# ## Plot in tmap ##
# 
# # Create all maps required
# map1 <- tm_shape(england) + 
#   tm_polygons(col = "ws1_all_condval", midpoint = 0, n=8)
# map2 <- tm_shape(resulthex) + 
#   tm_polygons(col = "ws1_all_condval", midpoint = 0, n=8)
# map3 <- tm_shape(england) + 
#   tm_polygons(col = "ws2_all_condval", midpoint = 0, n=8)
# map4 <- tm_shape(resulthex) + 
#   tm_polygons(col = "ws2_all_condval", midpoint = 0, n=8)
# 
# # Put together all plots into one
# current.mode <- tmap_mode("plot")
# tmap_arrange(map1, map2, map3, map4, ncol = 2)
# tmap_mode(current.mode)

## Plot in ggplot2 ##

# Define breaks for plotting
my_breaks <- c(-0.75, -0.25, -0.5, 0, 0.25, 0.5, 0.75, 1)

# Generate all plots
map1 <- ggplot(england) + 
  geom_sf(aes(fill = ws1_all_condval), lwd = 0, show.legend = NA) +
  scale_fill_viridis(name = "Conditional mean", breaks = my_breaks, labels = my_breaks, limits = c(-1,0.75)) +
  theme_void()

map2 <- ggplot(resulthex) + 
  geom_sf(aes(fill = ws1_all_condval), lwd = 0, show.legend = NA)  +
  scale_fill_viridis(name = "Conditional mean", breaks = my_breaks, labels = my_breaks, limits = c(-1,0.75)) +
  theme_void()

map3 <- ggplot(england) + 
  geom_sf(aes(fill = ws2_all_condval), lwd = 0, show.legend = NA)  +
  scale_fill_viridis(name = "Conditional mean", breaks = my_breaks, labels = my_breaks, limits = c(-1,0.75)) +
  theme_void()

map4 <- ggplot(resulthex) + 
  geom_sf(aes(fill = ws2_all_condval),  lwd = 0)  +
  scale_fill_viridis(name = "Conditional mean", breaks = my_breaks, labels = my_breaks, limits = c(-1,0.75)) +
  theme_void()

# Put together the plots into one
all_maps <- map1 + map2 + map3 + map4 + plot_layout(ncol = 2, guides = 'collect') + plot_annotation(tag_levels = 'A')
all_maps
ggsave(plot = all_maps, "./Plots/all_maps.jpeg", dpi = 300)

