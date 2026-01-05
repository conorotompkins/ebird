#setup ----
library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)
library(mclust)
library(broom)
library(workflows)
library(parsnip)
library(tidyclust)
library(factoextra)
library(clValid)

set.seed(123)

source("scripts/pull_ebirdst_metrics.R")

regions <- c("Pennsylvania", "Ohio", "West Virginia",
             "New Jersey", "New York", "Maryland",
             "Virginia", "Vermont", "New Hampshire",
             "Maine", "Connecticut", "Rhode Island",
             "Delaware", "Massachusetts") |>
  str_c(collapse = "|")

ne_states(iso_a2 = c("US", "CA")) |> 
  filter(str_detect(name, regions)) |> 
  select(name) |> 
  plot()

#load data ----
species_vec_all <- ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3, postbreeding_migration_quality == 3, prebreeding_migration_quality == 3) |> 
  filter(!str_detect(species_code, "example")) |> 
  pull(common_name)

metrics_list <- pmap(list(species_vec_all, "proportion-population", "3km", regions), pull_ebird_metrics)

metrics_df <- list_rbind(metrics_list)

metrics_df |>
  write_csv("output/combined_ebird_metrics.csv")

metrics_df <- read_csv("output/combined_ebird_metrics.csv")

metrics_df |> 
  filter(common_name == "Canada Warbler") |> 
  ggplot(aes(x, y, fill = prop_pop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void()

#process data ----
duplicate_geo <- metrics_df |>
  group_by(common_name, x, y) |>
  filter(n() > 1) |>
  ungroup() |>
  distinct(common_name)

duplicate_geo

metrics_df <- anti_join(metrics_df, duplicate_geo)

metrics_df |>
  summarize(zero_pct = mean(prop_pop == 0),
            .by = common_name) |>
  filter(zero_pct == 1)

occuring_species <- metrics_df |>
  summarize(zero_pct = mean(prop_pop == 0),
            .by = common_name) |>
  filter(zero_pct < 1.00) |>
  arrange(desc(zero_pct))

occuring_species

metrics_df <- metrics_df |>
  semi_join(occuring_species)

metrics_df |>
  write_csv("input/species_proportion_of_population.csv")

metrics_df |> 
  filter(common_name == "Redhead") |> 
  ggplot(aes(x, y, fill = prop_pop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))