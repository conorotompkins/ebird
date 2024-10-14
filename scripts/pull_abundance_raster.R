library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)

species_name <- "Yellow Warbler"

# download example data, yellow-bellied sapsucker in michigan
ebirdst_download_status(species = species_name,
                        download_abundance = TRUE,
                        #dry_run = TRUE,
                        pattern = "abundance_median_27km")

# load relative abundance raster stack with 52 layers, one for each week
abd <- load_raster(species_name, resolution = "27km", period = "weekly")

abd |> 
  select(1:10) |> 
  plot()

# load species specific mapping parameters
pars <- load_fac_map_parameters(species_name)
# custom coordinate reference system
crs <- st_crs(pars$custom_projection)
# legend breaks
breaks <- pars$weekly_bins
# legend labels for top, middle, and bottom
labels <- pars$weekly_labels

# the date that each raster layer corresponds to is stored within the labels
weeks <- as.Date(names(abd))
print(weeks)
#>  [1] "2022-01-04" "2022-01-11" "2022-01-18" "2022-01-25" "2022-02-01" "2022-02-08" "2022-02-15"
#>  [8] "2022-02-22" "2022-03-01" "2022-03-08" "2022-03-15" "2022-03-22" "2022-03-29" "2022-04-05"
#> [15] "2022-04-12" "2022-04-19" "2022-04-26" "2022-05-03" "2022-05-10" "2022-05-17" "2022-05-24"
#> [22] "2022-05-31" "2022-06-07" "2022-06-14" "2022-06-21" "2022-06-28" "2022-07-05" "2022-07-12"
#> [29] "2022-07-19" "2022-07-26" "2022-08-02" "2022-08-09" "2022-08-16" "2022-08-23" "2022-08-30"
#> [36] "2022-09-06" "2022-09-13" "2022-09-20" "2022-09-27" "2022-10-04" "2022-10-11" "2022-10-18"
#> [43] "2022-10-25" "2022-11-01" "2022-11-08" "2022-11-15" "2022-11-22" "2022-11-29" "2022-12-06"
#> [50] "2022-12-13" "2022-12-20" "2022-12-27"

# select a week in the middle of the year
abd <- abd[[20]]

# project to species specific coordinates
# the nearest neighbor method preserves cell values across projections
abd_prj <- project(trim(abd), crs$wkt, method = "near")

# get reference data from the rnaturalearth package
# the example data currently shows only the US state of Michigan
wh_states <- ne_states(country = c("United States of America"),
                       returnclass = "sf") |> 
  filter(name == "Pennsylvania") |> 
  st_transform(crs = crs) %>% 
  st_geometry()

plot(wh_states)

wh_states_raster <- wh_states |> 
  st_as_sf() |> 
  st_rasterize() |> 
  as_tibble() |> 
  as_spatraster()

#intersect(wh_states_raster, abd_prj)

abd_prj <- terra::crop(abd_prj, wh_states_raster, mask = TRUE, snap = "inside")

plot(abd_prj)

# start plotting
par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

# use raster bounding box to set the spatial extent for the plot
bb <- st_as_sfc(st_bbox(trim(abd_prj)))
plot(bb, col = "white", border = "white")
# add background reference data
plot(wh_states, col = "#cfcfcf", border = NA, add = TRUE)

# plot zeroes as light gray
plot(abd_prj, col = "#e6e6e6", maxpixels = ncell(abd_prj),
     axes = FALSE, legend = FALSE, add = TRUE)

# define color palette
pal <- ebirdst_palettes(length(breaks) - 1, type = "weekly")
# plot abundance
plot(abd_prj, col = pal, breaks = breaks, maxpixels = ncell(abd_prj),
     axes = FALSE, legend = FALSE, add = TRUE)

# state boundaries
plot(wh_states, add = TRUE, col = NA, border = "white", lwd = 1.5)

# legend
label_breaks <- seq(0, 1, length.out = length(breaks))
base_plot <- image.plot(zlim = c(0, 1), breaks = label_breaks, col = pal,
           smallplot = c(0.90, 0.93, 0.15, 0.85),
           legend.only = TRUE,
           axis.args = list(at = c(0, 0.5, 1), 
                            labels = round(labels, 2),
                            cex.axis = 0.9, lwd.ticks = 0))


tidy_plot <- ggplot() +
  geom_spatraster(data = abd_prj) +
  geom_sf(data = wh_states, alpha = 0, color = "black") +
  scale_fill_viridis_c()

tidy_plot