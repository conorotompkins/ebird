library(tidyverse)
library(lubridate)
library(rebird)
library(tigris)
library(sf)
library(rvest)
library(ggmap)
library(gt)
library(janitor)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

#download notable bird sightings
pgh_region_notables <- ebirdnotable(region = "US-PA",
                                    subnational2Code = 42,
                                    #lat = 40, lng = -80,
                                    back = 30) %>% 
  #st_as_sf(coords = c("lng", "lat"), crs = "epsg:3857")
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

pgh_region_notables %>% 
  ggplot() +
  geom_sf()

#get allegheny geometry
wpa_region <- tigris::counties(state = "PA", cb = TRUE) %>% 
  st_transform(crs = 4326) %>% 
  filter(NAME %in% c("Allegheny", "Beaver", "Westmoreland", 
                     "Butler", "Washington", "Armstrong",
                     "Lawrence", "Greene", "Fayette"))



#filter bird sightings to just allegheny county
local_bird_points <- st_filter(pgh_region_notables, wpa_region, .predicate = st_within)

local_bird_points %>% 
  group_by(comName) %>% 
  summarize(n = sum(howMany)) %>% 
  select(comName, n, contains("obs")) %>% 
  arrange(desc(n)) %>% 
  st_drop_geometry() %>% 
  rename(common_name = comName,
         observation_count = n) %>% 
  clean_names(case = "sentence") %>% 
  gt() %>% 
  tab_header(
    title = "Recent notable bird sightings in Western Pennsylvania"
  ) %>% 
  tab_source_note(
    source_note = "Data from eBird"
  )

wpa_region %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = local_bird_points, size = .5)


pgh_map <- get_map(location = c(lat = 40.445315, lon = -79.977104), 
                   zoom = 9,
                   maptype = "hybrid")
full_map <- ggmap(pgh_map) +
  geom_sf(data = wpa_region, fill = NA, 
          inherit.aes = FALSE,
          color = "white") +
  geom_sf(data = local_bird_points, 
          inherit.aes = FALSE, 
          shape = 1,
          size = 2,
          stroke = 1,
          color = "white") +
  theme_void()

full_map

#select top 10 notable sightings
top_birds <- local_bird_points %>% 
  group_by(comName, speciesCode) %>% 
  summarize(n = sum(howMany),
            last_seen = max(obsDt)) %>% 
  mutate(last_seen = str_extract(last_seen, "\\d{4}-\\d{2}-\\d{2}"),
         last_seen = ymd(last_seen)) %>% 
  ungroup() %>% 
  select(comName, speciesCode, n, last_seen) %>% 
  arrange(desc(n)) %>% 
  st_drop_geometry() %>% 
  slice(1:10)

#create dataframe with ebird links
ebird_site_links <- top_birds %>%
  mutate(ebird_link = str_c("https://ebird.org/species/", speciesCode))

#pull html for each site
ebird_site_links_html <- ebird_site_links %>%
  #slice(1:5) %>% 
  mutate(ebird_html = map(ebird_link, read_html))

ebird_site_links_html %>% 
  pull(ebird_html)

#create function that grabs preview image of a bird
get_bird_image <- function(url_html){
  
  img_link <- url_html %>% 
    html_node("[class='Media Media--hero Media--slides']") %>% 
    html_node("[class='Media-content']") %>% 
    html_nodes("img") %>% 
    html_attr("src") %>% 
    unlist() %>% 
    .[1]
  
  return(img_link)
}

test <- ebird_site_links_html %>% 
  mutate(bird_image = map_chr(ebird_html, possibly(get_bird_image, otherwise = NA_character_)))

bird_table <- test %>% 
  select(comName, ebird_link, n, last_seen, bird_image) %>%
  rename(observation_count = n,
         common_name = comName) %>% 
  mutate(hyperlink = str_c("[", common_name, "]", "(", ebird_link, ")")) %>% 
  select(hyperlink, observation_count, last_seen, bird_image) %>% 
  rename(common_name = hyperlink) %>% 
  clean_names(case = "sentence") %>%
  gt() %>% 
  text_transform(locations = cells_body(columns = vars(`Bird image`)), web_image) %>% 
  fmt_markdown(columns = vars(`Common name`))
