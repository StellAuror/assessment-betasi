pacman::p_load(
  sf,
  osmdata,
  dplyr
)


# Download major road data for Kraków from OpenStreetMap
road_data <- opq(bbox = "Kraków, Poland") %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# Extract line geometries representing roads
roads <- road_data$osm_lines

# Convert road lines into points (vertices) for distance calculations
road_points <- st_cast(roads, "POINT")

# Extract coordinates and other relevant information
road_points_df <- road_points %>%
  mutate(
    longitude = st_coordinates(geometry)[, 1],
    latitude = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%  
  select(osm_id, name, highway, longitude, latitude)

readr:::write_csv(road_points_df, "./data/OSM-Road.csv")

