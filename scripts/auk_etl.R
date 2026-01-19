library(tidyverse)
library(auk)
library(tictoc)
library(sf)

auk_file <- "input/ebd_US-PA_202401_202601_unv_smp_relNov-2025/ebd_US-PA_202401_202601_unv_smp_relNov-2025.txt"

output_file <- "input/pa_breeding_bird_atlas_data_raw.txt"

tic()
ebd <- auk_file |>
  auk_ebd() |>
  auk_date(date = c("2024-01-01", "2025-12-31")) |>
  auk_project("Pennsylvania Bird Atlas") |>
  auk_filter(file = output_file) |>
  # 4. read text file into r data frame
  read_ebd()
toc()
#944.459 sec elapsed

glimpse(ebd)

write_delim(ebd, "input/pa_breeding_bird_atlas_processed.txt", delim = "\t")

write_rds(ebd, "input/pa_breeding_bird_atlas_processed.rds")

ebd |> distinct(project_names)

ebd |>
  st_as_sf(coords = c("longitude", "latitude")) |>
  slice_sample(prop = .1) |>
  ggplot() +
  geom_sf(alpha = .01, size = .5) +
  theme_void()

ebd |>
  distinct(checklist_id) |>
  nrow()
