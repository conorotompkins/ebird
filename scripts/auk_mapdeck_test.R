library(tidyverse)
library(lubridate)
library(vroom)
library(auk)
library(janitor)
library(mapdeck)
library(hrbrthemes)

theme_set(theme_ipsum())

ebird_file <- "data/ebd_US-PA_201501_202011_relOct-2020/ebd_US-PA_201501_202011_relOct-2020.txt"
f_out <- "data/ebd_filtered.txt"


ebird_data <- ebird_file %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_species(species = "Dryocopus pileatus") %>% 
  auk_country(country = "United States") %>% 
  # 3. run filtering
  auk_filter(file = f_out) %>% 
  # 4. read text file into r data frame
  read_ebd()


vroom(f_out, n_max = 10) %>% 
  glimpse()

pa_data <- read_delim(f_out, 
                      delim = "\t",
                      col_types = cols(.default = "c"),
                      escape_double = TRUE) %>% 
  clean_names() %>% 
  mutate(observation_date = ymd(observation_date),
         observation_count = parse_number(observation_count),
         longitude = parse_number(longitude),
         latitude = parse_number(latitude)) %>% 
  arrange(observation_date)

glimpse(pa_data)

location_data <- pa_data %>% 
  select(longitude, latitude, observation_count) %>% 
  drop_na() 

glimpse(location_data)

location_data %>% 
  ggplot(aes(longitude, latitude)) +
  geom_density_2d_filled()


mapdeck(style = mapdeck_style('dark'), pitch = 0) %>% 
  add_screengrid(
    data = location_data
    , lat = "latitude"
    , lon = "longitude"
    , weight = "observation_count",
    , cell_size = 10
    , opacity = 0.3
    , colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )


  