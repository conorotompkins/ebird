library(tidyverse)
library(tidyclust)
library(shiny)
library(bslib)
library(rnaturalearth)
library(sf)
library(stars)
library(terra)
library(rmapshaper)
library(mapgl)

product_raster <- ebirdst::load_raster("Yellow Warbler", product = "proportion-population", resolution = "3km")

#raster_crs <- product_raster |> st_crs()

#file_path <- "kclust_shiny/cluster_assigments.csv"
file_path <- "cluster_assigments.csv"

regions <- c("Pennsylvania", "Ohio", "West Virginia",
             "New Jersey", "New York", "Maryland",
             "Virginia", "Vermont", "New Hampshire",
             "Maine", "Connecticut", "Rhode Island",
             "Delaware", "Massachusetts") |>
  str_c(collapse = "|")

map_region <- ne_states(iso_a2 = c("US", "CA")) |> 
  filter(str_detect(name, regions)) |> 
  summarize() |> 
  ms_simplify(keep = .3) |> 
  st_buffer(9500)

map_states <- ne_states(iso_a2 = c("US", "CA")) |> 
  filter(str_detect(name, regions))

region_centroid <- map_region |> 
  st_geometry() |> 
  st_transform(crs = 4326) |> 
  st_centroid() |> 
  st_coordinates() |> 
  round(1)

# ggplot() +
#   geom_sf(data = map_region, fill = "red") +
#   geom_sf(data = map_states) +
#   geom_point(data = region_centroid, aes(X, Y))

# define projection
crs_laea <- paste0("+proj=laea +lat_0=", region_centroid[2],
                   " +lon_0=", region_centroid[1])

map_region <- st_transform(map_region, crs_laea)

map_states <- st_transform(map_states, crs_laea)

# ggplot() +
#   geom_sf(data = map_region, fill = "red") +
#   geom_sf(data = map_states) +
#   geom_point(data = region_centroid, aes(X, Y))

#read in bird data
bird_df <- read_csv(file_path) |> 
  rename(cluster = .pred_cluster) |> 
  select(-.rownames) |> 
  st_as_sf(coords = c("x", "y"), crs = crs_laea)

# bird_df |> 
#   select(cluster) |> 
#   plot()

# bird_df |> 
#   ggplot() +
#   geom_sf(aes(color = cluster), size = .8) +
#   geom_sf(data = map_states, fill = NA) +
#   theme_void() +
#   theme(plot.background = element_rect(fill = "grey"))

#from perplexity
# 1. Assume df is your sf POINT dataframe
coords <- st_coordinates(bird_df)
tile_width <- min(diff(sort(unique(coords[, 1]))))
tile_height <- min(diff(sort(unique(coords[, 2]))))

# 2. Function to create a rectangle (tile) from a centroid
make_tile <- function(x, y, tile_width, tile_height) {
  x0 <- x - tile_width / 2
  x1 <- x + tile_width / 2
  y0 <- y - tile_height / 2
  y1 <- y + tile_height / 2
  list(
    matrix(
      c(x0, y0,
        x1, y0,
        x1, y1,
        x0, y1,
        x0, y0), 
      ncol = 2, byrow = TRUE
    )
  )
}

# 3. Create polygons for each centroid
tiles <- Map(
  make_tile,
  x = coords[, 1],
  y = coords[, 2],
  MoreArgs = list(tile_width = 3500, tile_height = 3500)
)

# 4. Convert to sf POLYGON
tiles_sf <- st_sf(
  bird_df, # retains original attributes
  geometry = st_sfc(
    lapply(tiles, st_polygon),
    crs = st_crs(bird_df)
  )
) |> 
  select(cluster)

#tiles_sf

# tile_cluster_map <- tiles_sf |> 
#   ggplot(aes(fill = cluster, color = cluster)) +
#   geom_sf(lwd = .1) +
#   geom_sf(data = map_states, fill = NA, inherit.aes = FALSE) +
#   theme_void() +
#   theme(plot.background = element_rect(fill = "lightgrey"))

# tile_cluster_map
# 
# tile_cluster_map +
#   facet_wrap(vars(cluster))

###

# str(tiles_sf, max.level = 1)
# st_crs(tiles_sf)

# mapboxgl(bounds = tiles_sf) |> 
#   add_fill_layer(id = "birds",
#                  source = tiles_sf,
#                  fill_color = "blue",
#                  fill_opacity = .5)

# Define UI for application that draws a histogram
ui <- page_fluid(
  
  layout_columns(
    col_widths = c(1, 3, 8),
    textOutput("click_tracker"),
    plotOutput("cluster_bar_chart", click = "plot_click"),
    mapboxglOutput("hclust_map", height = "100%")
  )
)


server <- function(input, output, session) {
  
  cluster_values <- tiles_sf |> 
    distinct(cluster) |> 
    mutate(cluster_int = parse_number(cluster)) |> 
    arrange(cluster_int) |> 
    pull(cluster)
  
  cluster_palette <- RColorBrewer::brewer.pal(n = length(cluster_values[1:12]), "Paired")
  cluster_palette <- c("grey", cluster_palette, "black", "pink")
  
  cluster_counts <- tiles_sf |> 
    st_drop_geometry() |> 
    count(cluster) |> 
    mutate(cluster = fct_reorder(cluster, n))
  
  output$cluster_bar_chart <- renderPlotly({
    {cluster_counts |> 
      ggplot(aes(x = n, y = cluster, fill = cluster)) +  # Use cluster for fill mapping
      geom_col(color = "grey") +
      scale_fill_manual(values = cluster_palette) +       # Use scale_fill_manual, not scale_fill_identity
      theme_bw()}
    |> ggplotly(source = "cluster_bar_chart")            # Native R pipe
  })
  
  output$hclust_map <- renderMapboxgl({
    mapboxgl(bounds = map_region) |> 
      add_fill_layer(
        id = "tiles",
        source = tiles_sf,
        fill_color = match_expr(
          "cluster",
          values = cluster_values,
          stops = cluster_palette
        ),
        fill_antialias = TRUE,
        fill_opacity = .9
      ) |> 
      add_line_layer(
        id = "states",
        source = map_states
      )
  })
  
  # Track Bar Click (cluster selection)
  selected_cluster <- reactive({
    click <- event_data("plotly_click", source = "cluster_bar_chart")
    if (!is.null(click)) {
      cluster_counts$cluster[click$pointNumber + 1]
    } else {
      NULL
    }
  })
  
  output$click_tracker <- renderText({
    if (!is.null(selected_cluster())) {
      paste("You clicked cluster:", selected_cluster())
    } else {
      "Click a bar to select cluster."
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
