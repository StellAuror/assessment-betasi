pacman::p_load(
  httr,
  jsonlite,
  dplyr,
  purrr
)

######## FIND CORRESPONDING DATA
# Load basic hotel data
dfBasic <- 
  readr::read_csv("data/BasicHotelData.csv")

# Extract place IDs based on coordinates
place_ids <- 
  lapply(1:nrow(dfBasic), function(i) {
    lat <- dfBasic$lat[i]
    lon <- dfBasic$lon[i]
    
    # Reverse geocoding API request
    geocode_url <- "https://maps.googleapis.com/maps/api/geocode/json"
    params <- list(
      latlng = paste(lat, lon, sep = ","),
      key = config::get("google_api_key")
    )
    
    response <- GET(geocode_url, query = params, add_headers(accept = "application/json"))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      
      # Identify exact matching place
      place_id <- data$results |> as.data.frame(stringsAsFactors = FALSE) |>
        select(place_id, formatted_address) |> 
        mutate(
          street = grepl(
            gsub("\\s+", " ", trimws(dfBasic[i, 9])), 
            gsub("\\s+", " ", trimws(formatted_address)), 
            ignore.case = TRUE
          ),
          code = grepl(
            gsub("\\s+", " ", trimws(dfBasic[i, 8])), 
            gsub("\\s+", " ", trimws(formatted_address)), 
            ignore.case = TRUE
          )
        ) |>
        filter(code, street) |> pull(1) |> head(1)
      
      c(dfBasic[[i, 2]], place_id)
      
    } else {
      c(dfBasic[[i, 2]], "error")
    }
  })

# Map ID pairs for Google & government data
place_ids |> 
  purrr::map_dfr(~{
    tibble::tibble(
      id_gov = .x[1],
      id_google = ifelse(length(.x) > 1, .x[2], NA)  
    )
  }) -> dfGooglePlace


############ GOOGLE REVIEW AND RATINGS

# API base URL
base_url <- "https://maps.googleapis.com/maps/api/place/details/json"

# Fetch Google reviews and ratings
results <- lapply(dfGooglePlace$id_google, function(place_id) {
  params <- list(
    place_id = place_id,
    fields = "name,rating,reviews,user_ratings_total,photos,price_level",
    key = config::get("google_api_key")
  )
  
  response <- GET(base_url, query = params, add_headers(accept = "application/json"))
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    
    # Extract key information with place ID
    place_info <- tryCatch({
      data$result %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        select(any_of(c(
          "name",
          "user_ratings_total", 
          "rating", 
          "reviews.author_name", 
          "reviews.rating", 
          "reviews.text", 
          "relative_time_description", 
          "reviews.profile_photo_url"
        ))) %>%
        mutate(id_google = place_id)
    }, error = function(e) {
      data.frame(id_google = place_id)
    })
    
    # Handle reviews with place ID
    reviews_df <- tryCatch({
      if (!is.null(data$result$reviews)) {
        data$result$reviews %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          select(any_of(c("author_name", "rating", "text", "time"))) %>%
          mutate(id_google = place_id)
      } else {
        data.frame(id_google = place_id)
      }
    }, error = function(e) {
      data.frame(id_google = place_id)
    })
    
    list(place_info = place_info, reviews = reviews_df)
    
  } else {
    print(paste("Error:", status_code(response), "in place_id:", place_id))
    list(place_info = data.frame(id_google = place_id), reviews = data.frame(id_google = place_id))
  }
})

# Merge and export data
left_join(
  dfGooglePlace,
  bind_rows(lapply(results, `[[`, "place_info")) |> unique(),
  join_by(id_google == id_google)
) |> readr::write_csv("./data/GoogleHotelData.csv")



####### FIND MOST POPULAR PLACES

# API base URL and parameters
base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
latitude <- 50.0619
longitude <- 19.9368
radius <- 10000  # Radius in meters

# Function to fetch paginated results
get_places <- function(location, radius, type, api_key) {
  all_results <- list()
  next_page_token <- NULL
  
  repeat {
    params <- list(
      location = location,
      radius = radius,
      type = type,
      key = api_key
    )
    
    # Add next_page_token if available
    if (!is.null(next_page_token)) {
      params$pagetoken <- next_page_token
      Sys.sleep(2)  # Recommended delay for next request
    }
    
    # API request
    response <- GET(base_url, query = params)
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # Append results
    all_results <- append(all_results, list(data$results))
    
    # Check for next page
    if (!is.null(data$next_page_token)) {
      next_page_token <- data$next_page_token
    } else {
      break
    }
  }
  
  # Combine results into a data frame
  do.call(rbind, lapply(all_results, function(x) {
    data.frame(
      name = x$name,
      rating = x$rating,
      user_ratings_total = x$user_ratings_total,
      vicinity = x$vicinity,
      stringsAsFactors = FALSE
    )
  }))
}

# Call the function
results <- get_places(
  location = paste(latitude, longitude, sep = ","),
  radius = radius,
  type = "tourist_attraction",
  api_key = config::get("google_api_key")
)

# Export results
readr::write_csv(results, "./data/GooglePopularPlaces.csv")

