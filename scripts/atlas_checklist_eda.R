library(tidyverse)
library(auk)
library(tictoc)
library(sf)

tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t")
toc()

# tic()
# ebd_df_rds <- read_rds("input/pa_breeding_bird_atlas_processed.rds") |>
#   st_as_sf(coords = c("longitude", "latitude"))
# toc()

glimpse(ebd_df)

ebd_df |>
  distinct(checklist_id)

atlas_blocks <- st_read("input/PABBA_2nd/PABBA_2nd.shp") |>
  select(NAME, NAME2, BLOCK_ID, REGION, ISPRIORITY) |>
  rename(BLOCK_ID_old = BLOCK_ID, BLOCK_ID = NAME)

glimpse(atlas_blocks)

checklist_coords <- ebd_df |>
  distinct(checklist_id, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE)

st_crs(checklist_coords) <- st_crs(atlas_blocks)

block_checklist_geo <- st_join(atlas_blocks, checklist_coords)

block_checklist_geo

block_summary <- left_join(ebd_df, block_checklist_geo) |>
  group_by(BLOCK_ID) |>
  summarize(
    checklist_count = n_distinct(checklist_id),
    species_observed = n_distinct(common_name),
    birders = n_distinct(observer_id),
    duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
    effort_distance_km = sum(effort_distance_km, na.rm = TRUE)
  ) |>
  ungroup()

block_summary <- left_join(atlas_blocks, block_summary)

glimpse(block_summary)

summary(block_summary$checklist_count)

quantile_probs <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .99)

test <- block_summary |>
  st_drop_geometry() |>
  as_tibble() |>
  reframe(
    quantile_value = quantile(
      checklist_count,
      probs = quantile_probs,
      na.rm = TRUE
    ),
    quantile = quantile_probs
  ) |>
  pull(quantile_value) |>
  round(-1)

test

names(test) <- NULL

block_summary |>
  ggplot() +
  geom_sf(aes(fill = checklist_count)) +
  scale_fill_viridis_b(breaks = test) +
  theme_void()

block_summary |>
  ggplot() +
  geom_sf(aes(fill = species_observed), lwd = 0) +
  scale_fill_viridis_c() +
  theme_void()

block_summary |>
  ggplot() +
  geom_sf(aes(fill = birders), lwd = 0) +
  scale_fill_viridis_c() +
  theme_void()

block_summary |>
  ggplot() +
  geom_sf(aes(fill = duration_hours), lwd = 0) +
  scale_fill_viridis_c() +
  theme_void()

block_summary |>
  ggplot() +
  geom_sf(aes(fill = effort_distance_km), lwd = 0) +
  scale_fill_viridis_c() +
  theme_void()

block_summary |>
  st_drop_geometry() |>
  as_tibble() |>
  select(checklist_count:effort_distance_km) |>
  GGally::ggpairs()
