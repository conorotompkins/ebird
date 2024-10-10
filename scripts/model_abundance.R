library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)
library(broom)

species_name <- "Yellow Warbler"

# download example data, yellow-bellied sapsucker in michigan
ebirdst_download_status(species = species_name,
                        download_abundance = TRUE,
                        #dry_run = TRUE,
                        pattern = "abundance_median_27km")

# load relative abundance raster stack with 52 layers, one for each week
abd <- load_raster(species_name, resolution = "27km", period = "weekly")


abd_df <- abd |> 
  terra::as.data.frame(cells = TRUE) |> 
  as_tibble()

abd_df <- abd_df |> 
  pivot_longer(-cell, names_to = "week", values_to = "abundance")

abd_df |> 
  group_by(cell) |> 
  slice_head(n = 2) |> 
  ggplot(aes(abundance)) +
  geom_histogram() +
  facet_wrap(vars(week))

model_df <- abd_df |> 
  mutate(week = as.factor(week)) |> 
  replace_na(list(abundance = 0))

model_df |> 
  summarize(abundance = mean(abundance), .by = cell) |> 
  arrange(desc(abundance))

test_df <- model_df |> 
  filter(cell == 566628)

lm(abundance ~ week, data = test_df) |> 
  tidy() |> 
  ggplot(aes(estimate)) +
  geom_histogram()

model_df <- model_df |> 
  group_nest(cell) |> 
  mutate(model = map(data, ~lm(abundance ~ week, data = .x)),
         coeff = map(model, tidy))

results <- model_df |> 
  unnest(coeff) |> 
  filter(term != "(Intercept)") |> 
  mutate(week = str_remove(term, "^week"),
         week = isoweek(week))

results |> 
  summarize(estimate = mean(estimate), .by = week) |> 
  ggplot(aes(week, estimate)) +
    geom_line()
