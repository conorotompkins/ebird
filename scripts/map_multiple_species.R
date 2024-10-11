library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)

source("scripts/pull_ebirdst_metrics.R")

species_vec <- c("Hooded Warbler", "Wood Duck")

test <- pull_ebird_metrics("Hooded Warbler", "abundance", "Pennsylvania")

test <- pmap(list(species_vec, "abundance", "Pennsylvania"), pull_ebird_metrics) |> 
  list_rbind()

test |> 
  ggplot(aes(x, y, fill = rel_abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(vars(species_name))

abd_sf <- test |> 
  st_as_sf(coords = c("x", "y"))

abd_sf |> 
  ggplot(aes(color = rel_abundance)) +
  geom_sf() +
  scale_color_viridis_c() +
  facet_wrap(vars(species_name))

test |> 
  pivot_wider(names_from = species_name, values_from = rel_abundance)