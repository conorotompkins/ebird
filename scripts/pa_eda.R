#pa eda
library(tidyverse)
library(lubridate)
library(vroom)
library(auk)
library(janitor)
library(mapdeck)
library(hrbrthemes)

theme_set(theme_ipsum())

f_out <- "data/ebd_filtered.txt"

pa_data <- read_delim(f_out, 
                      delim = "\t",
                      col_types = cols(.default = "c"),
                      escape_double = TRUE) %>% 
  clean_names() %>% 
  mutate(observation_date = ymd(observation_date),
         observation_count = parse_number(observation_count)) %>% 
  arrange(observation_date)

glimpse(pa_data)

pa_data %>% 
  select(observation_date, observation_count) %>% 
  group_by(observation_date) %>% 
  summarize(observation_count = sum(observation_count, na.rm = TRUE)) %>% 
  ggplot(aes(observation_date, observation_count)) +
  geom_point(alpha = .1) +
  geom_smooth(span = .1)

pa_data %>% 
  select(observation_date, observation_count) %>% 
  group_by(observation_date) %>% 
  summarize(observation_count = sum(observation_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = year(observation_date) %>% as.factor,
         yday = yday(observation_date)) %>% 
  ggplot(aes(yday, observation_count, color = year)) +
  #geom_line() +
  geom_smooth(span = .1)

pa_data %>% 
  mutate(year = year(observation_date) %>% as.factor) %>% 
  count(year, observer_id) %>% 
  ggplot(aes(n, fill = year)) +
  geom_histogram() +
  facet_wrap(~year, scales = "free")

pa_data %>% 
  mutate(year = year(observation_date) %>% as.factor,
         yday = yday(observation_date)) %>% 
  count(year, yday) %>% 
  mutate(cumulative_observations = cumsum(n)) %>% 
  group_by(year) %>% 
  mutate(year_label = case_when(yday == max(yday) ~ as.character(year),
                                TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  ggplot(aes(yday, cumulative_observations, color = year)) +
  geom_line() +
  geom_label(aes(label = year_label))

pa_data %>% 
  mutate(year = year(observation_date) %>% as.factor,
         yday = yday(observation_date)) %>% 
  count(year, yday) %>% 
  ggplot(aes(yday, n)) +
  geom_point()
