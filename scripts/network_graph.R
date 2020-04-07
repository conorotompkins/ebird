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

first(df$observation_date)
last(df$observation_date)

df_counts <- df %>% 
  count(common_name, name = "species_count", sort = TRUE)

df_pairs <- df %>% 
  pairwise_count(common_name, sampling_event_identifier, wt = observation_count) %>% 
  arrange(desc(n))
  
df_pairs %>% 
  slice(1:100) %>% 
  mutate(n = log10(n)) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(nodes) %>% 
  left_join(df_counts, by = c("name" = "common_name")) %>% 
  ggraph() +
    #geom_node_point() +
    geom_edge_fan(aes(edge_width = n, alpha = n)) +
    geom_node_label(aes(label = name, size = species_count)) +
    scale_edge_width_continuous(range = c(.5, 3)) +
    scale_edge_alpha_continuous(range = c(.2, 1)) +
    scale_size_continuous(range = c(.4, 5)) +
    #scale_node_size_continuous(range = c(.5, 2)) +
    theme_void()
