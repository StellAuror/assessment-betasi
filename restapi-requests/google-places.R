pacman::p_load(httr, jsonlite, dplyr, purrr, stringdist)

# Load basic hotel data
dfBasic <- readr::read_csv("data/BasicHotelData.csv")

### Function to fetch Google Places data based on name and address
fetch_google_place_id <- function(hotel_name, street, postal_code) {
  # Google Places API request
  places_url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"
  query <- paste(hotel_name, street, postal_code)
  
  params <- list(
    input = query,
    inputtype = "textquery",
    fields = "place_id,name,formatted_address",
    key = config::get("google_api_key")
  )
  
  response <- GET(places_url, query = params, add_headers(accept = "application/json"))
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    
    if (length(data$candidates) > 0) {
      matches <- data$candidates |> 
        as.data.frame(stringsAsFactors = FALSE) |>
        mutate(
          name_similarity = stringdist::stringdist(
            tolower(hotel_name), 
            tolower(name), 
            method = "jw"
          ),
          street_match = grepl(
            gsub("\\s+", " ", trimws(street)), 
            gsub("\\s+", " ", trimws(formatted_address)), 
            ignore.case = TRUE
          ),
          postal_code_match = grepl(
            gsub("\\s+", " ", trimws(postal_code)), 
            gsub("\\s+", " ", trimws(formatted_address)), 
            ignore.case = TRUE
          ),
          score = (1 - name_similarity) * 0.6 + 
            street_match * 0.2 + 
            postal_code_match * 0.2
        ) |> 
        arrange(desc(score)) |> 
        head(1)  # Pick the best match
      
      return(matches$place_id[1])
    }
  }
  return(NA)
}

### Apply the matching function to all hotels
dfGooglePlace <- dfBasic |> 
  rowwise() |> 
  mutate(
    id_google = fetch_google_place_id(
      hotel_name = content.name,
      street = content.street,
      postal_code = content.postalCode
    )
  ) |> 
  select(id_gov = content.rid, id_google)


print(dfGooglePlace, n = 1000)

############ GOOGLE REVIEW AND RATINGS
### Fetch Google reviews and ratings
base_url <- "https://maps.googleapis.com/maps/api/place/details/json"

results <- lapply(dfGooglePlace$id_google, function(place_id) {
  params <- list(
    place_id = place_id,
    fields = "name,rating,reviews,user_ratings_total,photos,price_level",
    key = config::get("google_api_key")
  )
  
  response <- GET(base_url, query = params, add_headers(accept = "application/json"))
  Sys.sleep(2)  # Small delay to allow token to activate
  
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


nrow(readr::read_csv("./data/GoogleHotelData.csv"))


####### FIND MOST POPULAR PLACES
### Function to fetch paginated results
# API base URL and parameters
base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
latitude <- 50.0619
longitude <- 19.9368
radius <- 10000  # Radius in meters

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
      latitude = x$geometry.location.lat,
      longitude = x$geometry.location.lng,
      stringsAsFactors = FALSE
    )
  }))
}

### Call the function
results <- get_places(
  location = paste(latitude, longitude, sep = ","),
  radius = radius,
  type = "tourist_attraction",
  api_key = config::get("google_api_key")
)

# Export results
readr::write_csv(results, "./data/GooglePopularPlaces.csv")

