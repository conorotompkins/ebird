library(tidyverse)
library(fields)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(stars)
library(tidyterra)

pull_ebird_metrics <- function(common_name, metric, resolution, region){
  
  ebirdst_download_status(common_name, dry_run = TRUE)
  
  #download data from ebird API
  ebirdst_download_status(species = common_name,
                          download_abundance = TRUE,
                          dry_run = TRUE,
                          pattern = "proportion-population_median_3km")

  ebirdst_download_status(species = common_name,
                          download_abundance = TRUE,
                          #dry_run = TRUE,
                          pattern = "proportion-population_median_3km")
  
  # load relative abundance raster for the full year
  product_raster <- load_raster(common_name, product = metric, resolution = resolution)
  
  product_raster
  
  #get regional boundaries
  region_boundary <- ne_states(iso_a2 = "US") |>
    filter(name == "Pennsylvania")
  
  region_boundary_proj <- st_transform(region_boundary, st_crs(product_raster))
  
  region_boundary_vect <- region_boundary |>
    st_transform(st_crs(product_raster)) |>
    vect()
  
  #crop to region, set value of cells outside of boundaries to NA
  product_raster_pa <- product_raster |> 
    crop(region_boundary_proj) |> 
    mask(region_boundary_proj)
  
  #plot(product_raster_pa)
  
  #take average
  product_raster_pa <- mean(product_raster_pa, na.rm = TRUE)
  
  #plot(product_raster_pa)
  
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
  product_raster_pa_laea <- project(product_raster_pa, crs_laea, method = "near") |> 
    # remove areas of the raster containing no data
    trim()
  
  # map the cropped and projected data
  #plot(product_raster_pa_laea, axes = FALSE, breakby = "cases")
  
  # ggplot() +
  #   geom_spatraster(data = abd_pa_laea) +
  #   scale_fill_viridis_c()
  
  product_df <- product_raster_pa_laea |> 
    as.data.frame(xy = TRUE) |> 
    as_tibble() |> 
    mutate(common_name = common_name) |> 
    rename(prop_pop = mean) |> 
    select(common_name, x, y, prop_pop)
  
  product_df
}