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

species_list <- c("Northern Cardinal", "Blue Jay")

df <- vroom("data/ebd_US-PA-003_201001_202003_relFeb-2020.zip", delim = "\t") %>% 
  clean_names() %>% 
  mutate_at(vars(observer_id, locality, observation_date, time_observations_started, protocol_type), str_replace_na, "NA") %>% 
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))),
         observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started, sep = "-")) %>% 
  filter(all_species_reported == 1)

df_top_protocols <- df %>% 
  count(protocol_type, sort = TRUE) %>% 
  slice(1:2)

df_pair_corr <- df %>% 
  semi_join(df_top_protocols) %>% 
  #mutate(observation_event_id = str_c(protocol_type, observation_event_id, sep = "-")) %>% 
  select(common_name, observation_event_id) %>% 
  arrange(observation_event_id) %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE)

df_slopegraph <- df_pair_corr %>% 
  filter(item1 %in% species_list) %>% 
  drop_na(correlation) %>% 
  arrange(item1, desc(correlation)) %>% 
  group_by(item1) %>% 
  slice(1:20)

df_corr_diff <- df_slopegraph %>% 
  pivot_wider(names_from = item1, values_from = correlation, names_prefix = "corr_") %>% 
  clean_names() %>% 
  mutate(corr_diff = abs(corr_blue_jay - corr_northern_cardinal)) %>% 
  select(item2, corr_diff)

df_slopegraph %>% 
  left_join(df_corr_diff) %>% 
  ggplot(aes(item1, correlation)) +
    geom_line(aes(group = item2, color = corr_diff), size = 2) +
    geom_point(size = 2) +
    #geom_text(data = filter(df_slopegraph, item1 == "Blue Jay"), aes(y = correlation, label = item2), nudge_x = -.3)
    geom_text_repel(data = filter(df_slopegraph, item1 == species_list[2]),
                    aes(y = correlation, label = item2), direction = "both", nudge_x = -.3, segment.alpha = .2) +
    geom_text_repel(data = filter(df_slopegraph, item1 == species_list[1]),
                    aes(y = correlation, label = item2), direction = "both", nudge_x = .3, segment.alpha = .2) +
    scale_color_viridis_c("Absolute difference in correlation") +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())

df_pair_corr_protocol_traveling <- df %>% 
  filter(protocol_type == "Traveling") %>% 
  #mutate(observation_event_id = str_c(protocol_type, observation_event_id, sep = "-")) %>% 
  select(common_name, observation_event_id) %>% 
  arrange(observation_event_id) %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE) %>% 
  mutate(protocol_type = "Traveling")

df_pair_corr_protocol_stationary <- df %>% 
  filter(protocol_type == "Stationary") %>% 
  #mutate(observation_event_id = str_c(protocol_type, observation_event_id, sep = "-")) %>% 
  select(common_name, observation_event_id) %>% 
  arrange(observation_event_id) %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE) %>% 
  mutate(protocol_type = "Stationary")

df_pair_corr_protocol_combined <- bind_rows(df_pair_corr_protocol_stationary, df_pair_corr_protocol_traveling)

df_slopegraph_protocol <- df_pair_corr_protocol_combined %>% 
  filter(item1 %in% species_list) %>% 
  drop_na(correlation) %>% 
  arrange(item1, protocol_type, desc(correlation)) %>% 
  group_by(item1, protocol_type) %>% 
  slice(1:20)

df_slopegraph_protocol %>% 
  ggplot(aes(item1, correlation)) +
  geom_point() +
  geom_line(aes(group = item2)) +
  #geom_text(data = filter(df_slopegraph, item1 == "Blue Jay"), aes(y = correlation, label = item2), nudge_x = -.3)
  geom_text_repel(data = filter(df_slopegraph_protocol, item1 == species_list[2]),
                  aes(y = correlation, label = item2), direction = "both", nudge_x = -.3, segment.alpha = .2) +
  geom_text_repel(data = filter(df_slopegraph_protocol, item1 == species_list[1]),
                  aes(y = correlation, label = item2), direction = "both", nudge_x = .3, segment.alpha = .2) +
  facet_wrap(~protocol_type) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
