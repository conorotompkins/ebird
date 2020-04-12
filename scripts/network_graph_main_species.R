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
  filter(common_name != "gull sp.") %>% 
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

plot <- graph_object %>% 
  mutate(name_label = case_when(str_detect(name, "Woodpecker") ~ name,
                                TRUE ~ as.character(NA))) %>% 
  ggraph() +
    geom_edge_link(aes(width = n, alpha = n)) +
    geom_node_point(aes(shape = !is.na(name_label))) +
    geom_node_label(aes(label = name_label), repel =  TRUE) +
    scale_edge_alpha(range = c(.1, .7)) +
    scale_edge_width(range = c(.3, 2)) +
    scale_shape_manual(values = c(1, 19)) +
    theme_void()

plot  

plot %>% 
  ggsave(filename = "output/network_graph_top_species.png")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        