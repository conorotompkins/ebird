library(tidyverse)
library(vroom)
library(janitor)

df <- vroom("data/ebd_sampling_relFeb-2020/ebd_sampling_relFeb-2020.txt/ebd_sampling_relFeb-2020.txt",
            n_max = 1000000) %>% 
  clean_names() %>% 
  filter(observation_date >= "2018-01-01",
         country == "United States")

list.files("data", full.names = TRUE, recursive = TRUE)

df

df %>% 
  distinct(country)

df %>% 
  distinct(state) %>% 
  View()

df %>% 
  distinct(country_code)

df %>% 
  summarize(date_max = max(observation_date, na.rm = TRUE),
            date_min = min(observation_date, na.rm = TRUE))

df %>% 
  ggplot(aes(observation_date)) +
    #geom_density()
    geom_histogram()


glimpse(df)

df %>% 
  filter(is.na(observation_date))

df %>% 
  drop_na(observer_id, observation_date, time_observations_started) %>% 
  count(observer_id, observation_date, time_observations_started, sort = TRUE) %>% 
  head(100) %>% 
  View()

df %>% 
  sample_n(10000, replace = FALSE) %>% 
  ggplot(aes(longitude, latitude)) +
    #geom_point(size = .1, alpha = .1) +
    geom_density_2d()
