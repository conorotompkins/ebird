library(tidyverse)
library(auk)
library(tictoc)
library(sf)

tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t") |>
  mutate(across(breeding_code), str_squish)
toc()

# tic()
# ebd_df_rds <- read_rds("input/pa_breeding_bird_atlas_processed.rds") |>
#   st_as_sf(coords = c("longitude", "latitude"))
# toc()

glimpse(ebd_df)

ebd_df |>
  count(breeding_category, breeding_code) |>
  arrange(breeding_category)

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

confirmed_species <- left_join(ebd_df, block_checklist_geo) |>
  filter(breeding_category == "C4") |>
  distinct(BLOCK_ID, common_name) |>
  count(BLOCK_ID, name = "confirmed_species")

confirmed_species

df_list <- list(atlas_blocks, block_summary, confirmed_species)

block_summary <- reduce(df_list, left_join, by = "BLOCK_ID")


plot_atlas_metric <- function(
  input_var,
  quantile_probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9)
) {
  graph_quantiles <- block_summary |>
    st_drop_geometry() |>
    as_tibble() |>
    reframe(
      quantile_value = quantile(
        {{ input_var }},
        probs = quantile_probs,
        na.rm = TRUE
      ),
      quantile = quantile_probs
    ) |>
    mutate(quantile_value = round(quantile_value, -1)) |>
    mutate(
      quantile_name = case_when(
        quantile_value == max(quantile_value) ~ str_c(">=", quantile_value),
        .default = as.character(quantile_value)
      )
    ) |>
    select(-quantile)

  quants <- graph_quantiles |>
    pull(quantile_value)

  names(quants) <- graph_quantiles$quantile_name

  print(quants)
  print(names(quants))

  print(length(quants) == length(names(quants)))

  legend_name <- label <- rlang::englue("{{input_var}}") |>
    str_replace("_", " ") |>
    str_to_title()

  block_summary |>
    ggplot() +
    geom_sf(aes(fill = {{ input_var }})) +
    scale_fill_viridis_b(breaks = quants) +
    labs(
      title = "Third Pennsylvania Bird Atlas",
      subtitle = "Checklist analysis",
      caption = "2024-2025",
      fill = legend_name
    ) +
    theme_void()
}

plot_atlas_metric(confirmed_species, c(.1, .2, .4, .6, .8, .9))

plot_atlas_metric(checklist_count)

plot_atlas_metric(species_observed)

plot_atlas_metric(birders, c(.1, .5, .9))

plot_atlas_metric(duration_hours)

plot_atlas_metric(effort_distance_km)

block_summary |>
  ggplot(aes(birders)) +
  geom_histogram()

block_summary |>
  st_drop_geometry() |>
  as_tibble() |>
  filter(confirmed_species == max(confirmed_species, na.rm = TRUE))

block_summary |>
  ggplot() +
  geom_sf(aes(fill = confirmed_species), lwd = 0) +
  scale_fill_viridis_c() +
  labs(
    title = "Third Pennsylvania Bird Atlas",
    subtitle = "Checklist analysis",
    caption = "2024-2025",
    fill = "Confirmed species"
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey"))

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
  select(checklist_count:confirmed_species) |>
  GGally::ggpairs() +
  theme_bw()
