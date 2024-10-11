library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)

pull_ebird_metrics <- function(species_name, metric, region){
  
  ebirdst_download_status(species_name, dry_run = TRUE)
  
  #download data from ebird API
  ebirdst_download_status(species = species_name,
                          download_abundance = TRUE,
                          dry_run = TRUE,
                          pattern = "abundance_full-year_mean_3km")
  
  ebirdst_download_status(species = species_name,
                          download_abundance = TRUE,
                          #dry_run = TRUE,
                          pattern = "abundance_full-year_mean_3km")
  
  # load relative abundance raster for the full year
  abd <- load_raster(species_name, product = metric, period = "full-year", resolution = "3km")
  
  #plot(abd)
  
  #get regional boundaries
  region_boundary <- ne_states(iso_a2 = "US") |>
    filter(name == "Pennsylvania")
  
  region_boundary_proj <- st_transform(region_boundary, st_crs(abd))
  
  region_boundary_vect <- region_boundary |>
    st_transform(st_crs(abd)) |>
    vect()
  
  #abd_pa <- crop(abd, pa_boundary)
  
  #crop to region, set value of cells outside of boundaries to NA
  abd_pa <- abd |> 
    crop(region_boundary_proj) |> 
    mask(region_boundary_proj)
  
  #plot(abd_pa)
  
  # ggplot() +
  #   geom_spatraster(data = abd_pa)
  
  #reproject raster to crs that fits region better
  region_centroid <- region_boundary |> 
    st_geometry() |> 
    st_transform(crs = 4326) |> 
    st_centroid() |> 
    st_coordinates() |> 
    round(1)
  
  # define projection
  crs_laea <- paste0("+proj=laea +lat_0=", region_centroid[2],
                     " +lon_0=", region_centroid[1])
  
  # transform to the custom projection using nearest neighbor resampling
  abd_pa_laea <- project(abd_pa, crs_laea, method = "near") |> 
    # remove areas of the raster containing no data
    trim()
  
  # map the cropped and projected data
  #plot(abd_pa_laea, axes = FALSE, breakby = "cases")
  
  # ggplot() +
  #   geom_spatraster(data = abd_pa_laea) +
  #   scale_fill_viridis_c()
  
  abundance_df <- abd_pa_laea |> 
    as.data.frame(xy = TRUE) |> 
    as_tibble() |> 
    mutate(species_name = species_name) |> 
    rename(rel_abundance = full_year) |> 
    select(species_name, x, y, rel_abundance)
  
  abundance_df
  
}

species_vec <- c("Hooded Warbler", "Wood Duck")

test <- pull_ebird_metrics("Hooded Warbler", "abundance", "Pennsylvania")

test <- pmap(list(species_vec, "abundance", "Pennsylvania"), pull_ebird_metrics) |> 
  list_rbind()

test |> 
  ggplot(aes(x, y, fill = rel_abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(vars(species_name))

abd_sf <- test |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs(crs(abd_pa_laea)) 

abd_sf |> 
  ggplot(aes(color = rel_abundance)) +
  geom_sf() +
  scale_color_viridis_c() +
  facet_wrap(vars(species_name))

test |> 
  pivot_wider(names_from = species_name, values_from = rel_abundance)