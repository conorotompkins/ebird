library(tidyverse)
library(lubridate)
library(janitor)
library(rebird)
library(leaflet)
library(mapdeck)
library(mapboxapi)
library(hrbrthemes)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum())

observations <- ebirdgeo(species_code("Dryocopus pileatus")) %>% 
  clean_names() %>% 
  mutate(obs_date = str_extract(obs_dt, "\\d{4}-\\d{2}-\\d{2}") %>% ymd) %>% 
  mutate(tooltip_info = str_c(com_name, loc_name, how_many, sep = "<br>"))

glimpse(observations)

observations %>% 
  group_by(obs_date) %>% 
  summarize(how_many = sum(how_many)) %>% 
  ggplot(aes(obs_date, how_many)) +
  geom_point()

observations %>% 
  mapdeck() %>% 
  add_scatterplot(lon = "lng",
                  lat = "lat",
                  radius = 100,
                  tooltip = "tooltip_info",
                  auto_highlight = TRUE
                  )
