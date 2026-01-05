library(tidyverse)
library(tidymodels)
library(tidyclust)
library(broom)
library(factoextra)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(tictoc)

set.seed(1234)

#get map
regions <- c("Pennsylvania", "Ohio", "West Virginia",
             "New Jersey", "New York", "Maryland",
             "Virginia", "Vermont", "New Hampshire",
             "Maine", "Connecticut", "Rhode Island",
             "Delaware", "Massachusetts") |>
  str_c(collapse = "|")

map_region <- ne_states(iso_a2 = c("US", "CA")) |> 
  filter(str_detect(name, regions)) |> 
  summarize() |> 
  ms_simplify(keep = .3) |> 
  st_buffer(9500)

plot(map_region)

map_states <- ne_states(iso_a2 = c("US", "CA")) |> 
  filter(str_detect(name, regions))

plot(map_states)

region_centroid <- map_region |> 
  st_geometry() |> 
  st_transform(crs = 4326) |> 
  st_centroid() |> 
  st_coordinates() |> 
  round(1)

ggplot() +
  geom_sf(data = map_region, fill = "red") +
  geom_sf(data = map_states)

#file_path <- "combined_ebird_metrics.csv"
file_path <- "input/species_proportion_of_population.csv"

bird_df <- read_csv(file_path) |> 
  rename(prop_pop_max = prop_pop) |> 
  mutate(prop_pop_max_log10 = log10(prop_pop_max + 1)) |> 
  select(-prop_pop_max)

dim(bird_df)

bird_df |>
  filter(common_name == "Willet") |>
  ggplot(aes(x, y, fill = prop_pop_max_log10)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

# bird_df |>
#   count(common_name, x, y) |>
#   filter(n > 1) |>
#   nrow() == 0

bird_df_wide <- bird_df |> 
  pivot_wider(names_from = common_name, values_from = prop_pop_max_log10)

bird_df_wide_noloc <- select(bird_df_wide, -c(x, y))

bird_df_loc <- bird_df_wide |> 
  select(x, y) 

## pca
pca_fit <- bird_df_wide_noloc |> 
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit |>
  tidy(matrix = "eigenvalues") |>
  filter(PC <= 10) |> 
  ggplot(aes(PC, percent)) +
  geom_col() +
  scale_x_continuous(breaks = c(1:100))

pca_fit |>
  tidy(matrix = "rotation") |> 
  pivot_wider(names_from = "PC", values_from = "value")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(3, "pt")
)

# plot rotation matrix
pca_rot <- pca_fit |>
  tidy(matrix = "rotation") |>
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") |> 
  rename(common_name = column)

pca_rot |> 
  select(common_name, PC1:PC3) |> 
  pivot_longer(-common_name) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(vars(name), scales = "free_y", ncol = 1)

pca_outliers <- pca_rot |>
  select(1:11) |> 
  pivot_longer(-common_name, names_to = "pc", values_to = "value") |> 
  group_by(pc) |> 
  filter(percent_rank(value) >= .99 | percent_rank(value) <= .01) |> 
  ungroup()

pca_outliers <- inner_join(pca_outliers, pca_rot)

pca_outliers |> 
  filter(pc == "PC1")

# pca_outliers_max <- pca_rot |> 
#   select(common_name, PC1:PC3) |> 
#   pivot_longer(-common_name) |> 
#   group_by(name) |> 
#   slice_max(order_by = value, n = 3) |> 
#   ungroup() |> 
#   distinct(common_name)
# 
# pca_outliers_min <- pca_rot |> 
#   select(common_name, PC1:PC3) |> 
#   pivot_longer(-common_name) |> 
#   group_by(name) |> 
#   slice_min(order_by = value, n = 3) |> 
#   ungroup() |> 
#   distinct(common_name)
# 
# pca_outliers <- bind_rows(pca_outliers_max, pca_outliers_min) |> 
#   left_join(pca_rot)

pca_rot |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC2")),
    color = "#904C2F"
  ) +
  ggrepel::geom_label_repel(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC2")),
    aes(label = common_name),
    color = "#904C2F"
  ) +
  tune::coord_obs_pred() +
  theme_bw()

pca_rot |>
  ggplot(aes(PC1, PC3)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC3")),
    color = "#904C2F"
  ) +
  ggrepel::geom_label_repel(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC3")),
    aes(label = common_name),
    color = "#904C2F"
  ) +
  tune::coord_obs_pred() +
  theme_bw()

pca_rot |>
  ggplot(aes(PC2, PC3)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers |> 
      filter(pc %in% c("PC2", "PC3")),
    color = "#904C2F"
  ) +
  ggrepel::geom_label_repel(
    data = pca_outliers |> 
      filter(pc %in% c("PC2", "PC3")),
    aes(label = common_name),
    color = "#904C2F"
  ) +
  tune::coord_obs_pred() +
  theme_bw()

pca_rot |>
  ggplot(aes(PC1, PC4)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC4")),
    color = "#904C2F"
  ) +
  ggrepel::geom_label_repel(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC4")),
    aes(label = common_name),
    color = "#904C2F"
  ) +
  tune::coord_obs_pred() +
  theme_bw()

pca_rot |>
  ggplot(aes(PC1, PC5)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC5")),
    color = "#904C2F"
  ) +
  ggrepel::geom_label_repel(
    data = pca_outliers |> 
      filter(pc %in% c("PC1", "PC5")),
    aes(label = common_name),
    color = "#904C2F"
  ) +
  tune::coord_obs_pred() +
  theme_bw()

bird_df |>
  semi_join(pca_outliers |> filter(pc == "PC5") |> mutate(common_name = fct_reorder(common_name, PC5))) |> 
  ggplot(aes(x, y, fill = prop_pop_max_log10)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(vars(common_name)) +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

df_loc_pca <- augment(pca_fit, bird_df_wide)

df_pca_long <- df_loc_pca |> 
  select(x, y, contains(".fitted")) |> 
  select(1:100) |> 
  pivot_longer(-c(x, y)) |> 
  mutate(name = str_remove(name, "^.fitted") |> parse_number())

plot_pcs <- function(x, pc){
  x |> 
    filter(name == pc) |> 
    ggplot(aes(x, y, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(direction = 1) +
    labs(title = str_c("PC ", pc)) +
    coord_fixed() +
    guides(fill = "none") +
    theme_void() +
    theme(panel.background = element_rect(fill = "black"))
}

plot_number <- 1:20

map(plot_number, ~plot_pcs(df_pca_long, .x)) |> 
  set_names(plot_number) |> 
  patchwork::wrap_plots()

plot_pcs(df_pca_long, 5)

#sample the data

df_sample <- df_loc_pca |> 
  slice_sample(prop = .5, weight_by = scales::rescale(.fittedPC5, c(0, 1))) |> 
  select(-starts_with(".fitted"), -c(.rownames))

glimpse(df_sample)

df_sample_pca <- df_loc_pca |> 
  slice_sample(prop = .5, weight_by = scales::rescale(.fittedPC1, c(0, 1))) |> 
  select(x, y, starts_with(".fitted")) |> 
  select(1:102)

glimpse(df_sample_pca)

df_sample_pca |> 
  ggplot(aes(x, y, fill = .fittedPC5)) +
  geom_tile(data = df_loc_pca, aes(x, y), inherit.aes = FALSE, fill = "grey") +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void()

# #tune
# birds_cv <- vfold_cv(df_sample_pca, v = 5)
# 
# hclust_spec <- hier_clust(num_clusters = tune())
# 
# cluster_rec <- recipe(~.,
#                        data = df_sample_pca
# )
# 
# hclust_wflow <- workflow(cluster_rec, hclust_spec)
# 
# clust_num_grid <- grid_regular(num_clusters(),
#                                levels = 10
# )
# 
# clust_num_grid
# 
# res <- tune_cluster(
#   hclust_wflow,
#   resamples = birds_cv,
#   grid = clust_num_grid,
#   control = control_grid(save_pred = TRUE, extract = identity),
#   metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
# )


## cluster

# hclust_wss_plot <- fviz_nbclust(df_sample |> select(-c(x, y)), FUN = hcut, method = "wss", k.max = 6)
# 
# hclust_wss_plot
# 
# hclust_wss_plot_pca <- fviz_nbclust(df_sample_pca |> select(-c(x, y)), FUN = hcut, method = "wss", k.max = 6)
# 
# hclust_wss_plot_pca

kclust_df <- tibble(kclust = c(2:20))

fit_cluster <- function(x){
  
  x |>
    fit(~ ., data = df_sample_pca |> select(-c(x, y)))
  
}

cluster_spec <- hier_clust(
  num_clusters = 15,
  linkage_method = "ward"
)

cluster_fit <- fit_cluster(cluster_spec)

plot(cluster_fit$fit, labels = FALSE)

kclust_df_test <- kclust_df |> 
  mutate(bird = list(df_sample_pca),
         fit = list(cluster_fit$fit),
         cluster_assignment = map2(fit, kclust, cutree),
         cluster_assignment = map(cluster_assignment, enframe, name = "row_id", value = "cluster")
  )

kclust_df_test |> 
  unnest(cluster_assignment)

# define projection
crs_laea <- paste0("+proj=laea +lat_0=", region_centroid[2],
                   " +lon_0=", region_centroid[1])

map_region <- st_transform(map_region, crs_laea)

map_states <- st_transform(map_states, crs_laea)

cluster_assigned <- kclust_df_test |> 
  filter(kclust %in% c(15)) |> 
  select(bird, kclust, cluster_assignment) |> 
  unnest(c(bird, cluster_assignment)) |> 
  mutate(cluster = as.factor(cluster)) 

cluster_assigned |> 
  count(cluster)

cluster_map <- cluster_assigned |> 
  ggplot(aes(x, y, fill = cluster)) +
  geom_tile() +
  geom_sf(data = map_states, inherit.aes = FALSE, color = "white", lwd = .5, alpha = 0.1, fill = "grey") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

cluster_map

cluster_map +
  facet_wrap(vars(cluster))

cluster_assigned |> 
  filter(cluster == 15) |> 
  ggplot(aes(x, y)) +
  geom_sf(data = map_states, inherit.aes = FALSE) +
  geom_tile() +
  theme_void()

#predict
chunked_list <- df_loc_pca |> 
  mutate(row_id = row_number(),
         chunk = (row_id %/% 500) + 1) |> 
  group_nest(chunk) |> 
  pull(data)

names(chunked_list) <- 1:length(chunked_list)

length(chunked_list)
str(chunked_list, max.level = 1)

chunked_list |> names()

# combined <- map(chunked_list$data, ~predict(cluster_fit, .x)) |>
#   list_rbind() |> 
#   bind_cols(df_loc_pca)

Sys.time()
combined_list <- list()
for (i in seq_along(chunked_list)){
  
  print(names(chunked_list[i]))
  tic()
  combined_list[[i]] <- predict(cluster_fit, chunked_list[[i]])
  toc()
  
}
Sys.time()

combined_df <- combined_list |> 
  list_rbind() |> 
  bind_cols(df_loc_pca)

combined_df |>
  ggplot(aes(x, y, fill = .pred_cluster)) +
  geom_sf(data = map_states, inherit.aes = FALSE) +
  geom_tile()

combined_df |> 
  write_csv("kclust_shiny/cluster_assigments.csv")