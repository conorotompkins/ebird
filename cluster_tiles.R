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

set.seed(1234)

source("scripts/pull_ebirdst_metrics.R")

species_vec_all <- ebirdst::ebirdst_runs |> 
  filter(is_resident == FALSE) |> 
  filter(breeding_quality == 3, nonbreeding_quality == 3) |> 
  filter(!str_detect(species_code, "example")) |> 
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

# abundance_list <- pmap(list(species_vec_all, "abundance", "Pennsylvania"), pull_ebird_metrics)
# 
# abundance_df <- list_rbind(abundance_list)
# 
# abundance_df |>
#   write_csv("output/combined_ebird_metrics.csv")

abundance_df <- read_csv("output/combined_ebird_metrics.csv")

duplicate_geo <- abundance_df |> 
  group_by(species_name, x, y) |> 
  filter(n() > 1) |>
  ungroup() |> 
  distinct(species_name)

duplicate_geo

abundance_df <- anti_join(abundance_df, duplicate_geo)

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

abundance_df |> 
  filter(rel_abundance == max(rel_abundance))

abundance_df |> 
  filter(species_name == "Snow Goose") |> 
  slice_sample(n = 5000) |> 
  pull(rel_abundance) |> 
  shapiro.test() |> 
  pluck("p.value")

test_normality <- function(x){
    
    x |> 
      slice_sample(n = 5000, weight_by = rel_abundance) |> 
      pull(rel_abundance) |> 
      shapiro.test() |> 
      purrr::pluck("p.value")
  
}

test_normality <- safely(test_normality, NA)

abundance_df |> 
  filter(species_name == "American Goldfinch") |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram()

abundance_df |> 
  filter(species_name == "American Goldfinch") |> 
  test_normality() |> 
  purrr::pluck("result")

#shapiro-wilk
norm_rs <- abundance_df |> 
  group_nest(species_name) |> 
  mutate(test = purrr::map(data, test_normality) |> purrr::map_dbl("result")) |> 
  select(-data)

#null hypothesis that data is normally distributed

#evidence that data is NOT normally distributed
norm_rs |> 
  filter(test < .05)

#least normal
norm_rs |> filter(test == min(test, na.rm = TRUE))

abundance_df |> 
  filter(species_name == "Red-winged Blackbird") |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram()

#most normal
norm_rs |> filter(test == max(test, na.rm = TRUE))

abundance_df |> 
  filter(species_name == "Dark-eyed Junco") |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram()

#cannot reject null
norm_rs |> 
  filter(test >= .05)

norm_rs |> 
  ggplot(aes(test)) +
  geom_histogram() +
  geom_vline(xintercept = .05)

graph_normality_test <- function(x, species){
  
  x_summary <- x |> 
    filter(species_name == species) |> 
    group_by(species_name) |> 
    summarize(mean = mean(rel_abundance), sd = sd(rel_abundance))
  
  res_sd1 <- x |> 
    filter(species_name == species) |> 
    left_join(x_summary) |> 
    mutate(mean_plus_sd = mean + sd,
           mean_minus_sd = mean - sd) |> 
    mutate(test = between(rel_abundance, mean_minus_sd, mean_plus_sd)) |> 
    summarize(mean(test) |> round(4)) |> 
    pull()
  
  res_sd1_bool <- res_sd1 >= .68
  
  res_sd2 <- x |> 
    filter(species_name == species) |> 
    left_join(x_summary) |> 
    mutate(mean_plus_sd = mean + (2 * sd),
           mean_minus_sd = mean - (2 * sd)) |> 
    mutate(test = between(rel_abundance, mean_minus_sd, mean_plus_sd)) |> 
    summarize(mean(test) |> round(4)) |> 
    pull()
  
  res_sd2_bool <- res_sd2 >= .95
  
  x |> 
    filter(species_name == species) |> 
    ggplot() +
    geom_histogram(aes(rel_abundance)) +
    geom_rect(data = x_summary,
              aes(xmin = mean - sd, xmax = mean + sd, ymin = 0, ymax = Inf),
              alpha = .3, fill = "red", color = NA) +
    geom_rect(data = x_summary,
              aes(xmin = mean - (2 * sd), xmax = mean + (2 * sd), ymin = 0, ymax = Inf),
              alpha = .1, fill = "red", color = NA) +
    geom_vline(data = x_summary, aes(xintercept = mean), color = "red") +
    labs(title = str_c("Normality test of", species, sep = " "),
         subtitle = str_c("68% of obs between mean +- 1 SD: ", res_sd1_bool, "(", res_sd1, ")", "\n",
                          "95% of obs between mean +- 2 SD: ", res_sd2_bool,"(", res_sd2, ")",
                          sep = ""))
  
}

graph_normality_test(abundance_df, "Dark-eyed Junco")

random_birds <- abundance_df |> 
  distinct(species_name) |> 
  slice_sample(n = 10)

test_sp <- c("Dark-eyed Junco", "Field Sparrow", "Common Grackle", "Snow Goose")

abundance_df |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

abundance_df |> 
  filter(species_name %in% test_sp) |> 
  group_by(species_name) |> 
  summarize(mean = mean(rel_abundance),
            median = median(rel_abundance),
            pct_95 = quantile(rel_abundance, .95),
            pct_99 = quantile(rel_abundance, .99))

abundance_df |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(log10(rel_abundance + 1))) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

abundance_df <- abundance_df |> 
  mutate(rel_abundance_scaled = rel_abundance |> scale() |> as.numeric(),
         rel_abundance_scaled_log10 = log10(rel_abundance + 1) |> scale() |> as.numeric(), .by = species_name)

abundance_df |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance_scaled)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance_scaled_log10)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df_wide <- abundance_df |> 
  select(species_name, x, y, rel_abundance_scaled_log10) |> 
  pivot_wider(names_from = species_name, values_from = rel_abundance_scaled_log10)

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

kclust <- kmeans(abundance_df_noloc, centers = 12)

tidy(kclust) |> select(size, withinss, cluster) |> arrange(desc(size))

cluster_geo <- augment(kclust, abundance_df_wide) |> 
  mutate(.cluster = fct_infreq(.cluster))

cluster_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile() +
  scale_fill_viridis_d()

cluster_geo |> 
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

cluster_geo |> 
  select(x, y, .cluster) |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile(data = blank_tiles, aes(x, y), inherit.aes = FALSE, fill = "light grey") +
  geom_tile() +
  scale_fill_viridis_d() +
  facet_wrap(vars(.cluster))

#hierarchical

wss_plot <- fviz_nbclust(abundance_df_noloc, FUN = hcut, method = "wss", k.max = 10)

wss_plot

wss_plot$data |> 
  mutate(diff = (y - lag(y)) / y) |> 
  ggplot(aes(clusters, diff, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_reverse()

# hclust_kclusts <- tibble(k = 1:2) %>%
#   mutate(
#     abundance = list(abundance_df_noloc),
#     kclust = purrr::map(k, ~hier_clust(num_clusters = .x, linkage_method = "ward")),
#     fit = purrr::map2(kclust, abundance, ~fit(.x, ~ ., .y)),
#     fit_summary = purrr::map(fit, extract_fit_summary),
#     score = purrr::map_dbl(fit_summary, "sse_total"),
#     dendrogram = purrr::map2(kclust, abundance, ~fit(.x, ~ ., .y)) |> plot(),
#     )
# 
# hclust_kclusts
# 
# hclust_kclusts |> 
#   filter(k == 3) |> 
#   pull(fit) |> 
#   pluck(1) |> 
#   plot()

hc_spec <- hier_clust(
  num_clusters = 7,
  linkage_method = "ward"
)

hc_spec

hc_fit <- hc_spec %>%
  fit(~ .,
      data = abundance_df_noloc
  )

hc_fit$fit |> str()

hc_fit$fit %>% plot()

hc_summary <- hc_fit %>% extract_fit_summary()

hc_summary %>% str()

hc_summary$n_members

hc_summary$sse_total

hc_preds <- extract_cluster_assignment(hc_fit)

#hc_preds <- hc_fit %>% predict(abundance_df_wide)

hclust_geo <- abundance_df_wide |> 
  select(x, y) |> 
  bind_cols(hc_preds) |> 
  mutate(.cluster = fct_infreq(.cluster))

hclust_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile() +
  scale_fill_viridis_d()

hclust_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile(data = blank_tiles, aes(x, y), fill = "light grey", inherit.aes = FALSE) +
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
  ggplot(aes(x, y, fill = .class)) +
  geom_tile()

gmm_clust |> 
  select(x, y, .class, .uncertainty) |> 
  ggplot(aes(x, y, fill = .class, alpha = .uncertainty)) +
  geom_tile() +
  scale_alpha_continuous(range = c(1, .1))

gmm_clust |> 
  select(x, y, .class, .uncertainty) |> 
  ggplot(aes(x, y, fill = .uncertainty)) +
  geom_tile() +
  scale_fill_viridis_c()