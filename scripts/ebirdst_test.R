library(tidyverse)
library(janitor)
library(ebirdst)
library(sf)
library(raster)
library(fasterize)
library(rnaturalearth)
library(lubridate)
library(tigris)
library(glue)
library(tictoc)
library(hrbrthemes)

#https://cornelllabofornithology.github.io/ebirdst/

options(timeout = max(300, getOption("timeout")))

get_abundance_table <- function(target_species_var){
  
  target_species <- ebirdst_runs %>% 
    filter(common_name == target_species_var) %>% 
    pull(species_code)
  
  glue("Pulling data for", target_species_var, "AKA", target_species, .sep = " ") %>% 
    message()
  
  tic()
  sp_path <- ebirdst_download(species = target_species, tifs_only = T, force = T)
  toc()
  
  message("Loading raster")
  abunds <- load_raster("abundance", path = sp_path)
  
  original_raster_crs <- raster::crs(abunds) %>% 
    as.character()
  
  message("Getting state polygon from Tigris")
  pa_shape <- states(cb = T) %>% 
    filter(NAME == "Pennsylvania") %>% 
    st_transform(crs = original_raster_crs)
  
  pa_bbox <- pa_shape %>% 
    sf::st_bbox(crs = original_raster_crs)
  
  message("Cropping raster")
  mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
  
  tic()
  abunds_cropped <- abunds %>% 
    crop(pa_bbox) %>% 
    projectRaster(crs = mollweide, method = "ngb")
  toc()
  
  message("Transforming raster to tibble")
  tic()
  abunds_table <- abunds_cropped %>% 
    as.data.frame(xy = T) %>% 
    as_tibble() %>% 
    pivot_longer(-c(x, y), names_to = "date", values_to = "value") %>% 
    mutate(date = str_remove(date, "^X"),
           date = str_replace_all(date, "\\.", "-"),
           date = ymd(date))
  toc()
  
  abunds_table
}

species_table <- tibble(species = c("Common Yellowthroat", "Indigo Bunting"))

species_table

species_table <- 
  species_table %>% 
  mutate(abundance_table = map(species, ~get_abundance_table(target_species_var = .x)))

abunds_table <- 
  species_table %>% 
  unnest(abundance_table)

target_species <- ebirdst_runs %>% 
  filter(common_name == "Common Yellowthroat") %>% 
  pull(species_code)

tic()
sp_path <- ebirdst_download(species = target_species, tifs_only = T)
toc()

sp_path

abunds <- load_raster("abundance", path = sp_path)

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_shape %>% 
  ggplot() +
  geom_sf()

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

tic()
abunds_cropped <- abunds %>% 
  crop(pa_bbox) %>% 
  projectRaster(crs = mollweide, method = "ngb")
toc()

tic()
abunds_table <- abunds_cropped %>% 
  as.data.frame(xy = T) %>% 
  as_tibble() %>% 
  pivot_longer(-c(x, y), names_to = "date", values_to = "value") %>% 
  mutate(date = str_remove(date, "^X"),
         date = str_replace_all(date, "\\.", "-"),
         date = ymd(date))
toc()

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

#find date with highest mean abundance per species, map that
abunds_table %>% 
  group_by(species, date) %>% 
  mutate(mean_value = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  filter(mean_value == max(mean_value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  geom_sf(data = pa_shape_moll, alpha = 0, color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(species~date, ncol = 1) +
  labs(fill = "Abundance") +
  theme_void()

abunds_table %>% 
  ggplot(aes(value, fill = species)) +
  geom_histogram(alpha = .5) +
  theme_ipsum()

abunds_table %>% 
  distinct(species)

top_areas <- abunds_table %>% 
  group_by(species, x, y) %>% 
  summarize(mean_abundance = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(species, desc(mean_abundance)) %>% 
  group_by(species) %>% 
  slice_head(n = 300) %>% 
  mutate(id = row_number() %>% as.character)

abunds_table %>% 
  inner_join(top_areas) %>% 
  mutate(id = fct_reorder(id, mean_abundance)) %>% 
  ggplot(aes(x = date, y = id, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~species)

mean_abunds_table <- abunds_table %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(species, month) %>% 
  summarize(mean_abundance = mean(value, na.rm = T)) %>% 
  ungroup()

mean_abunds_table %>% 
  ggplot(aes(x = month, y = mean_abundance, fill = species, color = species, group = species)) +
  geom_polygon(alpha = .5) +
  #facet_wrap(~species) +
  #scale_y_continuous(limits = c(0, max(mean_abunds_table$mean_abundance))) +
  coord_polar(theta = "x") +
  labs(title = "Species Abundance",
       subtitle = "2018 Pennsylvania",
       x = NULL,
       y = "Mean Abundance",
       fill = "Species",
       color = "Species") +
  theme_bw()











test %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  scale_fill_viridis_c()


##########
pis <- load_pis(sp_path, return_sf = T)

pis %>% glimpse()

extent <- ebirdst_extent(pa_bbox, t = c("2018-01-01", "2018-02-01"))

map_centroids(sp_path, ebirdst_extent(pa_bbox))

plot_pis(pis, ext = extent, by_cover_class = FALSE, n_top_pred = 15)

pis %>% 
  filter(model == "abd") %>% 
  st_as_sf() %>% 
  st_transform(mollweide) %>% 
  st_filter(x = ., y = pa_shape, join = st_intersects) %>%
  ggplot() +
  geom_sf(data = pa_shape, alpha = 0) +
  geom_sf(aes(color = stixel_prevalence)) +
  scale_color_viridis_c()


