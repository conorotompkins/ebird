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
  filter(n > 100) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

species_list <- c("Northern Cardinal", "Blue Jay")

df_species_nodes <- graph_object %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = row_number()) %>% 
  filter(name %in% species_list)


plot <- graph_object %>% 
  activate(nodes) %>% 
  mutate(name_label = case_when(name %in% species_list ~ name,
                                TRUE ~ as.character(NA))) %>% 
  activate(edges) %>% 
  left_join(df_species_nodes, by = c("from" = "node_id", "to" = "node_id")) %>% 
  mutate(species_flag = case_when(from %in% df_species_nodes$node_id | to %in% df_species_nodes$node_id ~ TRUE,
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

plot  

plot %>% 
  ggsave(filename = "output/network_graph_species_flag.png", width = 10, height = 10)
