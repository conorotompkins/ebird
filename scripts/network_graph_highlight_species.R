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
library(tidytext)

set.seed(1234)

theme_set(theme_ipsum())

df <- vroom("data/ebd_US-PA-003_201001_202003_relFeb-2020.zip", delim = "\t") %>% 
  clean_names() %>% 
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))),
         observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started)) %>% 
  filter(all_species_reported == 1)

df_counts <- df %>% 
  count(common_name, name = "species_count", sort = TRUE)

df_counts

df %>% 
  count(observation_date) %>% 
  mutate(recent_observation = year(observation_date) >= 2016) %>% 
  ggplot(aes(observation_date, n, color = recent_observation)) +
    geom_line()

df_pair_count <- df %>% 
  #filter(common_name != "gull sp.") %>% 
  pairwise_count(common_name, observation_event_id, wt = observation_count, diag = FALSE, upper = FALSE) %>% 
  arrange(desc(n)) %>% 
  drop_na(n)

df_pair_count

df_pair_corr <- df %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE)

df_pair_corr %>% 
  arrange(desc(correlation))

df_pair_corr %>% 
  ggplot(aes(correlation)) +
    geom_density()

#count
graph_object_count <- df_pair_count %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  filter(n > 100) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

species_list <- c("Northern Cardinal", "Blue Jay")

df_species_count_nodes <- graph_object %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = row_number()) %>% 
  filter(name %in% species_list)

plot_count <- graph_object_count %>% 
  activate(nodes) %>% 
  mutate(name_label = case_when(name %in% species_list ~ name,
                                TRUE ~ as.character(NA))) %>% 
  activate(edges) %>% 
  left_join(df_species_count_nodes, by = c("from" = "node_id", "to" = "node_id")) %>% 
  mutate(species_flag = case_when(from %in% df_species_count_nodes$node_id | to %in% df_species_count_nodes$node_id ~ TRUE,
                                  TRUE ~ FALSE),
         name = case_when(is.na(name) ~ "Other species",
                          TRUE ~ name)) %>%
  ggraph() +
    geom_edge_link(aes(width = n, color = name, alpha = species_flag)) +
    geom_node_point(aes(shape = !is.na(name_label))) +
    geom_node_label(aes(label = name_label), repel =  TRUE) +
    #facet_nodes(~name_label) +
    scale_edge_alpha_discrete(range = c(0.1, 1)) +
    scale_edge_color_manual(values = c("blue", "red", "black")) +
    scale_edge_width(range = c(.3, 3)) +
    scale_shape_manual(values = c(1, 19)) +
    theme_void()

plot_count  

plot_count %>% 
  ggsave(filename = "output/network_graph_count_species_flag.png", width = 10, height = 10)

#corr
graph_object_corr <- df_pair_corr %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  filter(abs(correlation) > .3) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

species_list <- c("Northern Cardinal", "Blue Jay")

df_species_corr_nodes <- graph_object_corr %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = row_number()) %>% 
  filter(name %in% species_list)

plot_corr <- graph_object_corr %>% 
  activate(nodes) %>% 
  mutate(name_label = case_when(name %in% species_list ~ name,
                                TRUE ~ as.character(NA))) %>% 
  activate(edges) %>% 
  left_join(df_species_corr_nodes, by = c("from" = "node_id", "to" = "node_id")) %>% 
  mutate(species_flag = case_when(from %in% df_species_corr_nodes$node_id | to %in% df_species_corr_nodes$node_id ~ TRUE,
                                  TRUE ~ FALSE),
         name = case_when(is.na(name) ~ "Other species",
                          TRUE ~ name)) %>%
  ggraph() +
    geom_edge_link(aes(width = correlation, color = name, alpha = species_flag)) +
    geom_node_point(aes(shape = !is.na(name_label))) +
    geom_node_label(aes(label = name_label), repel =  TRUE) +
    #facet_nodes(~name_label) +
    scale_edge_alpha_discrete(range = c(0.1, 1)) +
    scale_edge_color_manual(values = c("blue", "red", "black")) +
    scale_edge_width(range = c(.3, 3)) +
    scale_shape_manual(values = c(1, 19)) +
    theme_void()

plot_corr