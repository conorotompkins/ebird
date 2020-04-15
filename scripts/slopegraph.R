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

df_pair_corr <- df %>% 
  pairwise_cor(common_name, observation_event_id, diag = FALSE, upper = FALSE)

df_slopegraph <- df_pair_corr %>% 
  filter(item1 %in% species_list) %>% 
  drop_na(correlation) %>% 
  arrange(item1, desc(correlation)) %>% 
  group_by(item1) %>% 
  slice(1:20)

df_slopegraph %>% 
  ggplot(aes(item1, correlation)) +
  geom_point() +
  geom_line(aes(group = item2)) +
  #geom_text(data = filter(df_slopegraph, item1 == "Blue Jay"), aes(y = correlation, label = item2), nudge_x = -.3)
  geom_text_repel(data = filter(df_slopegraph, item1 == "Blue Jay"),
                  aes(y = correlation, label = item2), direction = "both", nudge_x = -.3, segment.alpha = .2) +
  geom_text_repel(data = filter(df_slopegraph, item1 == "Northern Cardinal"),
                  aes(y = correlation, label = item2), direction = "both", nudge_x = .3, segment.alpha = .2) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
