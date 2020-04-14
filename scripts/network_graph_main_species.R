library(tidyverse)
library(lubridate)
library(vroom)
library(janitor)
library(rebird)
library(hrbrthemes)
library(ggrepel)
library(gganimate)
library(widyr)
library(tidygraph)
library(ggraph)

theme_set(theme_ipsum())

df <- vroom("data/ebd_US-PA-003_201001_202003_relFeb-2020.zip") %>% 
  clean_names() %>% 
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))),
         observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started))

df_counts <- df %>% 
  count(common_name, name = "species_count", sort = TRUE)

df_counts

df_pairs <- df %>% 
  #filter(common_name != "gull sp.") %>% 
  pairwise_count(common_name, observation_event_id, wt = observation_count, diag = FALSE, upper = FALSE) %>% 
  arrange(desc(n)) %>% 
  drop_na(n)

df_pairs

graph_object <- df_pairs %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  filter(n > 50) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  left_join(df_counts, by = c("name" = "common_name"))

plot <- graph_object %>% 
    ggraph() +
    geom_edge_link(aes(width = n, alpha = n)) +
    geom_node_point(aes(size = species_count, alpha = species_count)) +
    scale_size_continuous("Total observations", labels = scales::comma) +
    scale_alpha_continuous("Total observations", labels = scales::comma) +
    scale_edge_alpha("Observations together", range = c(.1, .7), labels = scales::comma) +
    scale_edge_width("Observations together", range = c(.3, 2), labels = scales::comma) +
    theme_void()

plot  

plot %>% 
  ggsave(filename = "output/network_graph_top_species.png")

graph_object %>% 
  activate(nodes) %>% 
  #mutate(group = as.factor(group_infomap())) %>% 
  mutate(group = as.factor(group_edge_betweenness())) %>% 
  as_tibble() %>% 
  View()

plot_groups <- graph_object %>% 
  # activate(nodes) %>% 
  # mutate(group = as.factor(group_edge_betweenness())) %>% 
  activate(edges) %>% 
  mutate(group = as.factor(group_biconnected_component())) %>% 
  ggraph() +
    geom_edge_density(aes(fill = group)) +
    geom_edge_link(aes(width = n, alpha = n, color = group)) +
    geom_node_point(aes(size = species_count, alpha = species_count)) +
    scale_size_continuous("Total observations", labels = scales::comma) +
    scale_alpha_continuous("Total observations", labels = scales::comma) +
    scale_edge_alpha("Observations together", range = c(.1, .7), labels = scales::comma) +
    scale_edge_width("Observations together", range = c(.3, 2), labels = scales::comma) +
    guides(edge_color = FALSE, edge_fill = FALSE) +
    theme_void()

plot_groups %>% 
  ggsave(filename = "output/network_graph_attempt_color_fill.png")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        