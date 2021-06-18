library(tidyverse)
library(lubridate)
library(vroom)
library(auk)
library(janitor)
library(mapdeck)
library(hrbrthemes)

theme_set(theme_ipsum())

ebird_file <- "data/ebd_relOct-2020.tar"
f_out <- "data/ebd_filtered_test.txt"


list.files("data", recursive = TRUE)

test <- vroom("data/ebd_relOct-2020/ebd_relOct-2020.txt.gz", n_max = 100)

glimpse(test)

vroom("data/ebd_relOct-2020/ebd_relOct-2020.txt.gz", 
      delim = "\t",  
      n_max = 10^8,
      col_select = contains("DATE")) %>%
  clean_names() %>% 
  mutate(observation_date = ymd(observation_date)) %>% 
  filter(observation_date > "1990-01-01") %>% 
  count(observation_date) %>% 
  ggplot(aes(observation_date, n)) +
  geom_point()
  
vroom("data/ebd_relOct-2020/ebd_relOct-2020.txt.gz", n_max = 10^6) %>% 
  filter(`STATE CODE` == "US-PA") %>% 
  glimpse()

vroom("data/ebd_relOct-2020/ebd_relOct-2020.txt.gz", n_max = 10^6) %>% 
  count(`COUNTRY CODE`, sort = T)


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
