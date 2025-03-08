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

nclust <- 10

#https://medium.com/@ozturkfemre/unsupervised-learning-determination-of-cluster-number-be8842cdb11
fviz_dunn <- function(data, nclust) {
  k <- c(2:nclust)
  dunnin <- c()
  for (i in 2:nclust) {
    dunnin[i] <- dunn(distance = dist(data), clusters = kmeans(data, i)$cluster)
  }
  dunnin <- dunnin[2:nclust]
  plot(k, dunnin, xlab =  "Cluster number k",
       ylab = "Dunn Index",
       main = "Dunn Plot", cex.main=1,
       col = "dodgerblue1", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(dunnin==max(dunnin)) + 1, lwd=1, col="red", lty="dashed")
}

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

species_vec <- c("amecro")

# ebirdst_download_status(species = species_vec,
#                         download_abundance = TRUE,
#                         download_all = TRUE,
#                         force = TRUE,
#                         pattern = "proportion-population_median_3km")
# 
#ebirdst_download_status(species = "amecro", download_all = TRUE, dry_run = TRUE)

#metrics_list <- pmap(list(species_vec_all, "proportion-population", "3km", "Pennsylvania"), pull_ebird_metrics)

#metrics_df <- list_rbind(metrics_list)

# metrics_df |>
#    write_csv("output/combined_ebird_metrics.csv")

metrics_df <- read_csv("output/combined_ebird_metrics.csv")

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
  filter(zero_pct < 1) |> 
  arrange(desc(zero_pct))

occuring_species

occuring_species |> 
  write_csv("output/species_occur_pa.csv")

metrics_df <- metrics_df |> 
  semi_join(occuring_species)

metrics_df_scaled <- metrics_df |> 
  mutate(prop_pop_scaled = prop_pop |> scale() |> as.numeric(),
         prop_pop_rescaled = prop_pop |> scales::rescale(to = c(0, 1)),
         prop_pop_rescaled_log10 = log10(prop_pop_rescaled + 1), .by = common_name)

metrics_df_scaled |> 
  filter(is.nan(prop_pop_rescaled_log10)) |> 
  nrow() == 0

test_sp <- c("Dark-eyed Junco", "Field Sparrow", "Common Grackle", "Snow Goose", "American Crow")

metrics_df_scaled |>
  filter(common_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = prop_pop)) +
  geom_tile() +
  facet_wrap(vars(common_name)) +
  scale_fill_viridis_c()

metrics_df_scaled |>
  filter(common_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = prop_pop_scaled)) +
  geom_tile() +
  facet_wrap(vars(common_name)) +
  scale_fill_viridis_c()

metrics_df_scaled |>
  filter(common_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = prop_pop_rescaled_log10)) +
  geom_tile() +
  facet_wrap(vars(common_name)) +
  scale_fill_viridis_c()

metrics_df_wide <- metrics_df_scaled |> 
  select(common_name, x, y, prop_pop) |> 
  pivot_wider(names_from = common_name, values_from = prop_pop)

metrics_df_noloc <- select(metrics_df_wide, -c(x, y))

pca_fit <- metrics_df_noloc %>% 
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 10))

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
  #semi_join(random_birds, by = c("column" = "common_name")) |> 
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

sampled_sp <- metrics_df_scaled |> 
  select(common_name, prop_pop) |> 
  summarize(mean = mean(prop_pop), .by = common_name) |> 
  dplyr::slice_sample(n = 10, weight_by = mean)

metrics_df_noloc |>
  select(all_of(pull(sampled_sp, common_name))) |> 
  GGally::ggpairs(lower = list(continuous = my_fn))

## clustering ----

#kmeans ----
kclusts <- 
  tibble(k = 1:nclust) %>%
  mutate(
    kclust = purrr::map(k, ~kmeans(metrics_df_noloc, .x)),
    tidied = purrr::map(kclust, tidy),
    glanced = purrr::map(kclust, glance),
    augmented = purrr::map(kclust, augment, metrics_df_wide)
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

kmeans_wss_plot <- fviz_nbclust(metrics_df_noloc, FUN = kmeans, method = "wss", k.max = 20)

kmeans_wss_plot

kmeans_silhouette_plot <- fviz_nbclust(metrics_df_noloc, # data
                                       kmeans, # clustering algorithm
                                       method = "silhouette") # silhouette

kmeans_silhouette_plot

fviz_dunn(metrics_df_noloc, nclust = 5)

k <- c(2:5)
dunnin <- c()
for (i in 2:5) {
  dunnin[i] <- dunn(distance = dist(metrics_df_noloc), clusters = kmeans(metrics_df_noloc, i)$cluster)
}
dunnin <- dunnin[2:5]

dunnin_df <- tibble(k = k,
                    dunn = dunnin)

ggplot(dunnin_df, aes(k, dunnin)) +
  geom_line()

plot(k, dunnin, xlab =  "Cluster number k",
     ylab = "Dunn Index",
     main = "Dunn Plot", cex.main=1,
     col = "dodgerblue1", cex = 0.9 ,
     lty=1 , type="o" , lwd=1, pch=4,
     bty = "l",
     las = 1, cex.axis = 0.8, tcl  = -0.2)
abline(v=which(dunnin==max(dunnin)) + 1, lwd=1, col="red", lty="dashed")

p1 <- assignments |> 
  ggplot(aes(x, y)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

nclust_kmeans <- 5

kmeans_spec <- k_means(num_clusters = nclust_kmeans)

kclust <- kmeans_spec |> 
  fit(~ .,
      data = metrics_df_noloc
  )

kclust

#kclust <- kmeans(metrics_df_noloc, centers = nclust_kmeans)

tidy(kclust) |> select(size, withinss, cluster) |> arrange(desc(size))

#metrics
#sse_total
kclust$betweenss / kclust$totss

cluster_geo <- metrics_df_wide |> 
  bind_cols(extract_cluster_assignment(kclust)) |> 
  select(.cluster, everything())

#custom_palette <- c(RColorBrewer::brewer.pal(12, "Paired"), "grey")

custom_palette_kmeans <- RColorBrewer::brewer.pal(nclust_kmeans, "Paired")

kmeans_map <- cluster_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_kmeans) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by kmeans algorithm",
       caption = "Data from eBird Status and Trends",
       fill = "Cluster") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 12))

kmeans_map

ggsave("output/kmeans_map.png", kmeans_map, width = 20, height = 12, dpi = 300)

global_avg <- metrics_df |> 
  summarize(prop_pop_global_mean = mean(prop_pop),
            .by = c(common_name))

global_avg

cluster_avg <- cluster_geo |> 
  pivot_longer(cols = -c(x, y, .cluster), names_to = "common_name") |> 
  summarize(cluster_avg = mean(value),
            .by = c(.cluster, common_name))

cluster_avg |> 
  left_join(global_avg) |> 
  mutate(diff = cluster_avg - prop_pop_global_mean) |> 
  group_by(.cluster) |> 
  slice_max(order_by = diff, n = 3)

cluster_geo |> 
  pivot_longer(cols = -c(x, y, .cluster)) |> 
  summarize(value = mean(value), .by = c(.cluster, name)) |> 
  arrange(.cluster, desc(value)) |> 
  group_by(.cluster) |> 
  slice_head(n = 3) |> 
  ungroup() |> 
  select(-value) |> 
  pivot_wider(names_from = .cluster, values_from = name) |> 
  unnest(everything()) #|> 
#view()

blank_tiles <- metrics_df_wide |>
  distinct(x, y)

kmeans_map_facet <- cluster_geo |> 
  select(x, y, .cluster) |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile(data = blank_tiles, aes(x, y), inherit.aes = FALSE, fill = "light grey", alpha = .5) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_kmeans) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by kmeans algorithm",
       caption = "Data from eBird Status and Trends") +
  guides(fill = "none") +
  facet_wrap(vars(.cluster)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14))

kmeans_map_facet

ggsave("output/kmeans_map_facet.png", width = 20, height = 12, dpi = 300)

#hierarchical ----
hclust_wss_plot <- fviz_nbclust(metrics_df_noloc, FUN = hcut, method = "wss", k.max = 20)

hclust_wss_plot

hclust_silhouette_plot <- fviz_nbclust(metrics_df_noloc, # data
                                       hcut, # clustering algorithm
                                       method = "silhouette") # silhouette

hclust_silhouette_plot

fviz_dunn(metrics_df_noloc, nclust = 10)

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

nclust_hclust <- 4

hc_spec <- hier_clust(
  num_clusters = nclust_hclust,
  linkage_method = "ward"
)

hc_spec

hc_fit <- hc_spec %>%
  fit(~ .,
      data = metrics_df_noloc
  )

hc_fit$fit |> str()

hc_fit$fit %>% plot()

hc_summary <- hc_fit %>% extract_fit_summary()

hc_summary %>% str()

hc_summary$n_members

hc_summary$sse_total

hc_preds <- extract_cluster_assignment(hc_fit)

#hc_preds <- hc_fit %>% predict(abundance_df_wide)

hclust_geo <- metrics_df_wide |> 
  select(x, y) |> 
  bind_cols(hc_preds) |> 
  mutate(.cluster = fct_infreq(.cluster))

#custom_palette_hclust <- c(RColorBrewer::brewer.pal(9, "Paired"), "dark grey", "black")

custom_palette_hclust <- RColorBrewer::brewer.pal(nclust_hclust, "Paired")

hclust_map <- hclust_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_hclust) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by hclust algorithm",
       caption = "Data from eBird Status and Trends",
       fill = "Cluster") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 12))

hclust_map

ggsave("output/hclust_map.png", hclust_map, width = 20, height = 12, dpi = 300)

hclust_map_facet <- hclust_geo |> 
  ggplot(aes(x, y, fill = .cluster)) +
  geom_tile(data = blank_tiles, aes(x, y), fill = "light grey", alpha = .5, inherit.aes = FALSE) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_hclust) +
  facet_wrap(vars(.cluster)) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by hclust algorithm",
       caption = "Data from eBird Status and Trends") +
  guides(fill = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 12))

hclust_map_facet

ggsave("output/hclust_map_facet.png", hclust_map_facet, width = 20, height = 12, dpi = 300)

#GMM
gmm_kclust <- tibble(kclust = 1:10,
                     data = list(metrics_df_noloc)) |> 
  mutate(gmm_res = purrr::map2(data, kclust, ~Mclust(data = .x, G = .y)),
         gmm_metrics = purrr::map(gmm_res, glance))

gmm_kclust |> 
  unnest(gmm_metrics) |> 
  ggplot(aes(x = kclust, y = BIC)) +
  geom_line() +
  geom_point() +
  #scale_y_reverse() +
  scale_x_continuous(breaks = c(1:10))

nclust_gmm <- gmm_kclust |> 
  unnest(gmm_metrics) |> 
  filter(BIC == min(BIC)) |> 
  pull(kclust)

tictoc::tic()
m <- Mclust(metrics_df_noloc, G = nclust_gmm)
tictoc::toc()

m

tidy(m)

glance(m)

gmm_clust <- augment(m, metrics_df_wide)

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
  geom_tile() +
  theme_void()

gmm_clust |> 
  select(x, y, .class, .uncertainty) |> 
  ggplot(aes(x, y, fill = .class, alpha = .uncertainty)) +
  geom_tile() +
  scale_alpha_continuous(range = c(1, .1)) +
  theme_void()

gmm_clust |> 
  select(x, y, .class, .uncertainty) |> 
  ggplot(aes(x, y, fill = .uncertainty)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void()

custom_palette_gmm <- RColorBrewer::brewer.pal(nclust_gmm, "Paired")

gmm_map <- gmm_clust |> 
  ggplot(aes(x, y, fill = .class)) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_gmm) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by GMM algorithm",
       caption = "Data from eBird Status and Trends",
       fill = "Cluster") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 12))

gmm_map

ggsave("output/gmm_map.png", gmm_map, width = 20, height = 12, dpi = 300)

gmm_map_facet <- gmm_clust |> 
  ggplot(aes(x, y, fill = .class)) +
  geom_tile(data = blank_tiles, aes(x, y), fill = "light grey", inherit.aes = FALSE) +
  geom_tile() +
  scale_fill_manual(values = custom_palette_gmm) +
  facet_wrap(vars(.class)) +
  labs(title = "Types of habitat inferred from species proportion of population",
       subtitle = "Clusters determined by GMM algorithm",
       caption = "Data from eBird Status and Trends") +
  guides(fill = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 12))

gmm_map_facet

ggsave("output/gmm_map_facet.png", gmm_map_facet, width = 20, height = 12, dpi = 300)