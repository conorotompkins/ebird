library(tidyverse)
library(janitor)
library(ebirdst)
library(sf)
library(raster)
library(fasterize)
library(rnaturalearth)
library(lubridate)

ebirdst_runs %>% 
  filter(common_name == "Pileated Woodpecker") %>% 
  pull(species_code)

sp_path <- ebirdst_download(species = "example_data")

sp_path

abunds <- load_raster("abundance", path = sp_path)

date_vector <- parse_raster_dates(abunds)
print(date_vector)

abund <- abunds[[26]]
#rm(abunds)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
abund_moll <- projectRaster(abunds, crs = mollweide, method = "ngb")

wh_states <- ne_states(country = c("United States of America", "Canada"),
                       returnclass = "sf") %>% 
  st_transform(crs = mollweide) %>% 
  st_geometry()

#fasterize(wh_states, abund_moll)



wh_states %>% 
  ggplot() +
  geom_sf()

abund_moll %>% 
  ggplot() +
  geom_raster()

fasterize(abund_moll, wh_states)

# calculate ideal color bins for abundance values for this week
week_bins <- calc_bins(abund_moll)

# start plotting
par(mfrow = c(1, 1), mar = c(0, 0, 0, 6))

bb <- st_as_sfc(st_bbox(trim(abund_moll)))

bb %>% 
  ggplot() +
  geom_sf()

abund_moll %>% 
  raster() %>% 
  ggplot() +
  geom_sf()



raster_tibble <- as.data.frame(abund_moll, xy = TRUE) %>% 
  as_tibble() %>% 
  pivot_longer(-c(x, y), names_to = "date", values_to = "value")


raster_tibble %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  scale_fill_viridis_c()

top_areas <- raster_tibble %>% 
  group_by(x, y) %>% 
  summarize(mean_abundance = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(mean_abundance)) %>% 
  slice_head(n = 100) %>% 
  mutate(id = row_number() %>% as.character)

raster_tibble %>% 
  inner_join(top_areas) %>% 
  mutate(date = str_remove(date, "^X"),
         date = str_replace_all(date, "\\.", "-"),
         date = ymd(date)) %>% 
  mutate(id = fct_reorder(id, mean_abundance)) %>% 
  ggplot(aes(x = date, y = id, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c()

raster_tibble %>% 
  #inner_join(top_areas) %>% 
  mutate(date = str_remove(date, "^X"),
         date = str_replace_all(date, "\\.", "-"),
         date = ymd(date)) %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(month) %>% 
  summarize(mean_abundance = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = month, y = mean_abundance, group = 1)) +
  geom_polygon() +
  coord_polar() +
  theme_bw()
