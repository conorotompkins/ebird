library(tidyverse)
library(lubridate)
library(vroom)
library(auk)
library(janitor)
library(mapdeck)
library(hrbrthemes)

theme_set(theme_ipsum())

ebird_file <- "data/ebd_relOct-2020/ebd_relOct-2020.txt.gz"
f_out <- "data/ebd_filtered_test.txt"


f <- function(x, pos) filter(x, `COUNTRY CODE` == "US")
test_file <- read_tsv_chunked(ebird_file, DataFrameCallback$new(f), chunk_size = 1000)
