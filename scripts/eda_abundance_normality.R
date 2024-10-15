library(tidyverse)

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
  semi_join(norm_rs |> filter(test == min(test, na.rm = TRUE))) |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram()

#most normal
norm_rs |> filter(test == max(test, na.rm = TRUE))

abundance_df |> 
  semi_join(norm_rs |> filter(test == max(test, na.rm = TRUE))) |> 
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
  group_by(species_name) |> 
  summarize(mean = mean(rel_abundance),
            median = median(rel_abundance),
            pct_95 = quantile(rel_abundance, .95),
            pct_99 = quantile(rel_abundance, .99))

abundance_df_scaled <- abundance_df |> 
  mutate(rel_abundance_scaled = rel_abundance |> scale() |> as.numeric(),
         rel_abundance_rescaled = rel_abundance |> scales::rescale(to = c(0, 1)),
         rel_abundance_rescaled_log10 = log10(rel_abundance_rescaled + 1), .by = species_name)

#histogram
abundance_df_scaled |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(rel_abundance)) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

abundance_df_scaled |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(rel_abundance_scaled)) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

abundance_df_scaled |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(rel_abundance_rescaled)) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

abundance_df_scaled |> 
  filter(species_name %in% test_sp) |> 
  ggplot(aes(rel_abundance_rescaled_log10)) +
  geom_histogram() +
  facet_wrap(vars(species_name), scales = "free", ncol = 1)

#maps
abundance_df_scaled |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df_scaled |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance_scaled)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()

abundance_df_scaled |>
  filter(species_name %in% test_sp) |> 
  ggplot(aes(x, y, fill = rel_abundance_rescaled_log10)) +
  geom_tile() +
  facet_wrap(vars(species_name)) +
  scale_fill_viridis_c()