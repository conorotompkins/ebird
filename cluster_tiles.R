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

set.seed(1234)

source("scripts/pull_ebirdst_metrics.R")

species_vec_all <- ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3) |> 
  pull(common_name)

species_vec_warbs <- ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(str_detect(common_name, "Warbler")) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3) |> 
  filter(!(common_name %in% c("Black-throated Gray Warbler", "Lucy's Warbler", "MacGillivray's Warbler", "Virginia's Warbler",
                              "Townsend's Warbler", "Hermit Warbler", "Golden-cheeked Warbler"))) |> 
  pull(common_name)

species_vec_grassland <- ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3) |> 
  filter(common_name %in% c("Bobolink", "Grasshopper Sparrow", "Field Sparrow")) |> 
  pull(common_name)

ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3) |> 
  filter(str_detect(common_name, "Sandpiper|Snipe|Woodcock"))

#species_vec <- c(species_vec_warbs, species_vec_grassland)

ebirdst_download_status(species = species_vec_all[3],
                        download_abundance = TRUE,
                        dry_run = TRUE)

abundance_list <- pmap(list(species_vec_all, "abundance", "Pennsylvania"), pull_ebird_metrics)

abundance_df <- list_rbind(abundance_list)

duplicate_geo <- abundance_df |> 
  group_by(species_name, x, y) |> 
  filter(n() > 1) |>
  ungroup() |> 
  distinct(species_name)

duplicate_geo

abundance_df <- anti_join(abundance_df, duplicate_geo)

random_birds <- abundance_df |> 
  distinct(species_name) |> 
  slice_sample(n = 10)

abundance_df |>
  semi_join(random_birds) |> 
  ggplot(aes(x, y, fill = rel_abundance)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df |> 
  summarize(zero_pct = mean(rel_abundance == 0),
            .by = species_name) |> 
  filter(zero_pct == 1)

occuring_species <- abundance_df |> 
  summarize(zero_pct = mean(rel_abundance == 0),
            .by = species_name) |>
  filter(zero_pct < 1) |> 
  arrange(desc(zero_pct))

occuring_species

abundance_df <- abundance_df |> 
  semi_join(occuring_species)

abundance_df <- abundance_df |> 
  mutate(rel_abundance_scaled = scale(rel_abundance) |> as.numeric(), .by = species_name)

abundance_df |> 
  semi_join(random_birds) |> 
  ggplot(aes(x = rel_abundance_scaled)) +
  geom_density() +
  facet_wrap(vars(species_name), scales = "free")

abundance_df_wide <- abundance_df |> 
  select(-rel_abundance) |> 
  pivot_wider(names_from = species_name, values_from = rel_abundance_scaled)

abundance_df_noloc <- select(abundance_df_wide, -c(x, y))

pca_fit <- abundance_df_noloc %>% 
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit %>%
  tidy(matrix = "rotation")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  #semi_join(random_birds, by = c("column" = "species_name")) |> 
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  #xlim(-1, 1) + ylim(-1, 1) +
  #coord_fixed() + # fix aspect ratio to 1:1
  tune::coord_obs_pred() +
  cowplot::theme_minimal_grid(12)

my_fn <- function(data, mapping, ...){
  # Using default ggplot density function
  
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=after_stat(density)), geom="tile", contour = FALSE) +
    scale_fill_gradientn(colours=viridis::viridis(100, option="viridis"))
  p
}

# abundance_df_noloc |> 
#   GGally::ggpairs(lower = list(continuous = my_fn))

## clustering

#kmeans
nclust <- 14

kclusts <- 
  tibble(k = 1:nclust) %>%
  mutate(
    kclust = purrr::map(k, ~kmeans(abundance_df_noloc, .x)),
    tidied = purrr::map(kclust, tidy),
    glanced = purrr::map(kclust, glance),
    augmented = purrr::map(kclust, augment, abundance_df_wide)
  )

clusters <- kclusts |>
  unnest(cols = c(tidied))

assignments <- kclusts |>
  unnest(cols = c(augmented))

clusterings <- kclusts |>
  unnest(cols = c(glanced))

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1:nclust))

p1 <- assignments |> 
  ggplot(aes(x, y)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

kclust <- kmeans(abundance_df_noloc, centers = 6)

augment(kclust, abundance_df_wide) |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile() +
  scale_fill_viridis_d()

augment(kclust, abundance_df_wide) |> 
  pivot_longer(cols = -c(x, y, .cluster)) |> 
  summarize(value = mean(value), .by = c(.cluster, name)) |> 
  arrange(.cluster, desc(value)) |> 
  group_by(.cluster) |> 
  slice_head(n = 3) |> 
  ungroup() |> 
  select(-value) |> 
  pivot_wider(names_from = .cluster, values_from = name) |> 
  unnest(everything()) |> 
  view()

blank_tiles <- abundance_df_wide |>
  distinct(x, y)

augment(kclust, abundance_df_wide) |> 
  select(x, y, .cluster) |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile(data = blank_tiles, aes(x, y), inherit.aes = FALSE, fill = "light grey") +
  geom_tile() +
  scale_fill_viridis_d() +
  facet_wrap(vars(.cluster))

#GMM
tictoc::tic()
m <- Mclust(abundance_df_noloc, G = 5)
tictoc::toc()

m

tidy(m)

glance(m)

gmm_clust <- augment(m, abundance_df_wide)

gmm_clust |> 
  summarize(.uncertainty = mean(.uncertainty),
            sd = sd(.uncertainty),
            .by = .class)

gmm_clust |> 
  ggplot(aes(.uncertainty)) +
  geom_histogram() +
  facet_wrap(vars(.class), scales = "free")

gmm_clust |> 
  select(x, y, .class, .uncertainty) |> 
  ggplot(aes(x, y, fill = .class, alpha = .uncertainty)) +
  geom_tile() +
  scale_alpha_continuous(range = c(1, .1))
  
