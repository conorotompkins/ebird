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


df_pair_count %>% 
  filter(item1 %in% species_list) %>% 
  arrange(item1, desc(n)) %>% 
  group_by(item1) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(item2 = reorder_within(x = item2, by = n, within = item1)) %>% 
  ggplot(aes(n, item2, fill = item1)) +
    geom_col(alpha = .9) +
    facet_wrap(~item1, scales = "free_y") +
    scale_y_reordered() +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Shared observations",
         y = NULL)

df_pair_corr <- df %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE)

df_pair_corr %>% 
  filter(item1 %in% species_list) %>% 
  drop_na(correlation) %>% 
  arrange(item1, desc(correlation)) %>% 
  group_by(item1) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(item2 = reorder_within(x = item2, by = correlation, within = item1)) %>% 
  ggplot(aes(correlation, item2, fill = item1)) +
  geom_col(alpha = .9) +
  facet_wrap(~item1, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Correlation",
       y = NULL)
