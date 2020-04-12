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

# ebirdgeo(lat = 40, lng = -80)
# 
# df_freq_raw <- ebirdfreq(loctype = 'states', loc = 'US-PA', startyear = 2019,
#                          endyear = 2019, startmonth = 1, endmonth = 12)
# 
# ebirdregion(loc = 'US-OH', max = 10, provisional = TRUE, hotspot = TRUE)

df <- vroom("data/ebd_US-PA-003_201001_202003_relFeb-2020.zip") %>% 
  clean_names() %>% 
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))),
         observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started))

# df %>% 
#   distinct(county)
# 
# first(df$observation_date)
# last(df$observation_date)

df_counts <- df %>% 
  count(common_name, name = "species_count", sort = TRUE)

df_counts

df_pairs <- df %>% 
  filter(common_name != "gull sp.") %>% 
  pairwise_count(common_name, observation_event_id, wt = observation_count, diag = FALSE, upper = FALSE) %>% 
  arrange(desc(n)) %>% 
  drop_na(n) #%>% 
  # semi_join(df_counts, by = c("item1" = "common_name")) %>% 
  # semi_join(df_counts, by = c("item2" = "common_name"))

df_pairs

graph_object <- df_pairs %>% 
  as_tbl_graph(directed = FALSE)

df_edges <- graph_object %>% 
  activate(edges) %>% 
  as_tibble()

df_edges_main <- df_edges %>% 
  filter(n > 500)

df_nodes <- graph_object %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = row_number())

df_nodes_main <- df_nodes %>% 
  semi_join(df_edges_main, by = c("node_id" = "from")) %>% 
  semi_join(df_edges_main, by = c("node_id" = "to"))

df_nodes_main


plot <- graph_object %>% 
  activate(edges) %>% 
  filter(n > 100) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  mutate(name_label = case_when(str_detect(name, "Woodpecker") ~ name,
                                TRUE ~ as.character(NA))) %>% 
  ggraph() +
    geom_edge_link(aes(width = n,
                      alpha = n)) +
    geom_node_point(aes(shape = !is.na(name_label))) +
    geom_node_label(aes(label = name_label), repel =  TRUE) +
    scale_edge_alpha(range = c(.1, .7)) +
    scale_edge_width(range = c(.3, 2)) +
    scale_shape_manual(values = c(1, 19)) +
    theme_void()
  

plot %>% 
  ggsave(filename = "output/network_graph_top_species.png")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        