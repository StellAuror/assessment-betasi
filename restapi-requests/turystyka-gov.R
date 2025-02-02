pacman::p_load(
  httr,
  jsonlite,
  dplyr
)

# Reuest - URL & params
url <- "https://api.turystyka.gov.pl/registers/open/cwoh"
params <- list(
  city = "Kraków",
  size = 220
)

# GET request to obtain a data
response <- GET(url, query = params, add_headers(accept = "application/json"))

# Response validation
if (status_code(response) == 200) {
  dfBasic <- fromJSON(content(response, as = "text", encoding = "UTF-8")) |>
    as.data.frame()
} else {
  print(paste("Error:", status_code(response)))
}

# Select only important data

dfBasic |> select(1:14, -9, -8) |>
  tidyr::unnest_wider(content.spatialLocation) |>
  mutate(
    lat = purrr::map_dbl(coordinates, ~ .x[[1]][1]),
    lon = purrr::map_dbl(coordinates, ~ .x[[1]][2])
  ) |> select(-13) -> dfBasic

# save data
readr::write_csv(dfBasic, "./data/BasicHotelData.csv")

