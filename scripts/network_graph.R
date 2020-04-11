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
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))))

df %>% 
  distinct(county)

first(df$observation_date)
last(df$observation_date)

df_counts <- df %>% 
  count(common_name, name = "species_count", sort = TRUE) %>% 
  top_n(5)

df_counts

df <- df %>% 
  mutate(observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started))



df_pairs <- df %>% 
  filter(common_name != "gull sp.") %>% 
  pairwise_count(common_name, observation_event_id, wt = observation_count, diag = FALSE, upper = FALSE) %>% 
  arrange(desc(n)) %>% 
  drop_na(n) %>% 
  filter(n >= 10)

df_pairs <- df_pairs %>% 
  filter(item1 == "Blue Jay" | item1 == "Northern Cardinal" | item2 == "Blue Jay" | item2 == "Northern Cardinal")
  
graph_object <- df_pairs %>% 
  as_tbl_graph(directed = FALSE)

df_node_ids <- graph_object %>% 
  activate(nodes) %>% 
  mutate(id = row_number()) %>% 
  as_tibble()

target_node_id <- df_node_ids %>% 
  filter(name == "Blue Jay" | name == "Northern Cardinal") %>% 
  pull(id)


graph_object %>% 
  activate(edges) %>% 
  filter(from %in% c(target_node_id)) %>%
  ggraph() +
    #geom_node_point() +
    geom_edge_fan(aes(edge_width = n, 
                      alpha = n,
                      color = as.factor(from)
    )) +
    geom_node_point() +
    #geom_node_label(aes(label = name)) +
    scale_color_manual(values = c("red", "blue")) +
    theme_void()