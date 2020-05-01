library(tidyverse)
library(lubridate)
library(janitor)
library(hrbrthemes)
library(vroom)
library(ggrepel)
library(gganimate)
library(tidyquant)

set.seed(1234)

theme_set(theme_bw(base_size = 16))


df <- vroom("data/ebd_US-PA-003_201001_202003_relFeb-2020.zip", delim = "\t") %>% 
  clean_names() %>% 
  mutate_at(vars(observer_id, locality, observation_date, time_observations_started, protocol_type), str_replace_na, "NA") %>% 
  mutate(observation_count = as.numeric(str_replace(observation_count, "X", as.character(NA))),
         observation_event_id = str_c(observer_id, locality, observation_date, time_observations_started, sep = "-"),
         observation_date = ymd(observation_date)) %>%
  filter(all_species_reported == 1)

df_top_protocols <- df %>% 
  count(protocol_type, sort = TRUE) %>% 
  slice(1:2)

df <- df %>% 
  semi_join(df_top_protocols) %>% 
  filter(year(observation_date) >= 2016)

df_species_count <- df %>% 
  group_by(common_name) %>% 
  summarize(observation_count = sum(observation_count, na.rm = TRUE)) %>% 
  arrange(desc(observation_count)) %>% 
  slice(1:10)

df_cumulative <- df %>% 
  semi_join(df_species_count, by = c("common_name")) %>% 
  group_by(common_name, observation_date) %>% 
  summarize(observation_count = sum(observation_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(common_name, observation_date) %>% 
  group_by(common_name) %>% 
  mutate(observation_count_cumulative = cumsum(observation_count)) %>% 
  tq_mutate(
    # tq_mutate args
    select     = observation_count,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 21,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "mean_21"
  )

df_cumulative %>% 
  ggplot(aes(observation_date, observation_count_cumulative, group = common_name)) +
  geom_line(alpha = .5) +
  geom_label(data = df_cumulative %>% filter(observation_date == last(observation_date)), 
             aes(label = common_name)) +
  scale_y_comma()

plot <- df_cumulative %>% 
  ggplot(aes(observation_date, observation_count_cumulative, group = common_name)) +
  geom_line(alpha = .5) +
  geom_segment(aes(xend = last(df_cumulative$observation_date) + 240, yend = observation_count_cumulative), linetype = 2, colour = 'grey') +
  geom_point(aes(size = mean_21)) +
  # geom_label_repel(aes(x = last(df_cumulative$observation_date) + 120, label = common_name),
  #                  #hjust = -.1,
  #                  #vjust = 0,
  #                  nudge_x = 50,
  #                  #direction = "x"
  # ) +
  geom_label(aes(x = last(df_cumulative$observation_date) + 210, label = common_name), size = 6) +
  scale_y_comma() +
  scale_size_continuous("21 day rolling average of observation count", range = c(2, 10), labels = scales::comma) +
  scale_x_date(limits = c(first(df_cumulative$observation_date), last(df_cumulative$observation_date) + 250)) +
  labs(x = NULL,
       y = "Cumulative observations",
       title = "eBird observations in Allegheny County",
       subtitle = "Top 10 birds 2016 through January 2020",
       caption = "@conor_tompkins, data from eBird") +
  coord_cartesian(clip = 'off') +
  #theme(#plot.margin = margin(5.5, 300, 5.5, 5.5),
  #      axis.text = element_text(size = 30)) +
  #theme_bw() +
  transition_reveal(observation_date)


#animate(plot, width = 1200, height = 900, end_pause = 5, duration = 15, fps = 3)
anim_save(filename = "cumulative_observations.gif", path = "output", animation = plot, width = 1600, height = 900, end_pause = 10, duration = 15, fps = 20)
