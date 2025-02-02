# Install required packages if not already installed
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")

# Load packages
library(httr)
library(jsonlite)

# Function to download air quality station data
download_air_quality_data <- function(api_key, date_from = NULL, date_to = NULL) {
  base_url <- "https://api.openaq.org/v3/locations"
  bbox_coords <- "19.85,50.00,20.10,50.15"  # Bounding box for Krakow area
  
  query_params <- list(
    bbox = bbox_coords,
    limit = 100
  )
  if (!is.null(date_from)) query_params$date_from <- date_from
  if (!is.null(date_to)) query_params$date_to <- date_to
  
  response <- GET(
    url = base_url,
    add_headers(`X-API-Key` = api_key),
    query = query_params
  )
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    if (!is.null(json_data$results) && length(json_data$results) > 0) {
      results <- json_data$results
      
      # Create a data frame with station information
      air_quality_data <- data.frame(
        id = results$id,
        name = results$name,
        country = results$country.name,
        provider = results$provider.name,
        latitude = results$coordinates.latitude,
        longitude = results$coordinates.longitude,
        datetime_first = results$datetimeFirst.utc,
        datetime_last = results$datetimeLast.utc,
        stringsAsFactors = FALSE
      )
      return(air_quality_data)
    } else {
      stop("No data available for the specified area.")
    }
  } else {
    error_message <- content(response, as = "text", encoding = "UTF-8")
    stop("Error fetching station data: ", status_code(response), " - ", error_message)
  }
}

# Function to download sensors for a given station
download_sensors_for_location <- function(api_key, location_id) {
  base_url <- sprintf("https://api.openaq.org/v3/locations/%s/sensors", location_id)
  query_params <- list(limit = 100)
  
  response <- GET(
    url = base_url,
    add_headers(`X-API-Key` = api_key),
    query = query_params
  )
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    if (!is.null(json_data$results) && length(json_data$results) > 0) {
      return(json_data$results)
    } else {
      message("No sensors found for location ID: ", location_id)
      return(NULL)
    }
  } else {
    error_message <- content(response, as = "text", encoding = "UTF-8")
    stop("Error fetching sensors for location ", location_id, ": ", status_code(response), " - ", error_message)
  }
}

# Function to download measurements for a given sensor
download_measurements_for_sensor <- function(api_key, sensor_id, date_from = NULL, date_to = NULL) {
  base_url <- sprintf("https://api.openaq.org/v3/sensors/%s/measurements", sensor_id)
  query_params <- list(limit = 100)
  if (!is.null(date_from)) query_params$date_from <- date_from
  if (!is.null(date_to)) query_params$date_to <- date_to
  
  response <- GET(
    url = base_url,
    add_headers(`X-API-Key` = api_key),
    query = query_params
  )
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    if (!is.null(json_data$results) && length(json_data$results) > 0) {
      return(json_data$results)
    } else {
      message("No measurements found for sensor ID: ", sensor_id)
      return(NULL)
    }
  } else {
    error_message <- content(response, as = "text", encoding = "UTF-8")
    stop("Error fetching measurements for sensor ", sensor_id, ": ", status_code(response), " - ", error_message)
  }
}

### Main Script

# Enter your API key
api_key <- config::get("open-aq-key")

# Define the date range
date_from <- "2018-11-21T00:00:00Z"
date_to <- "2018-11-22T00:00:00Z"

# Download station data
cat("Downloading station data...\n")
air_quality_data <- download_air_quality_data(api_key, date_from, date_to)
print("Station list:")
print(air_quality_data)

# Download sensors and measurements for each station
all_measurements <- list()

for (i in 1:nrow(air_quality_data)) {
  station_id <- air_quality_data$id[i]
  station_name <- air_quality_data$name[i]
  cat("Downloading sensors for station: ", station_name, " (ID: ", station_id, ")\n")
  
  sensors <- tryCatch({
    download_sensors_for_location(api_key, station_id)
  }, error = function(e) {
    cat("Error fetching sensors for station ID:", station_id, " - ", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(sensors) && nrow(as.data.frame(sensors)) > 0) {
    for (j in 1:nrow(sensors)) {
      sensor_id <- sensors$id[j]
      cat("Downloading measurements for sensor ID: ", sensor_id, "\n")
      measurements <- tryCatch({
        download_measurements_for_sensor(api_key, sensor_id, date_from, date_to)
      }, error = function(e) {
        cat("Error fetching measurements for sensor ID:", sensor_id, " - ", e$message, "\n")
        return(NULL)
      })
      key <- paste0("station_", station_id, "_sensor_", sensor_id)
      all_measurements[[key]] <- measurements
    }
  } else {
    message("No sensors found for station ID: ", station_id)
  }
}

# Display measurements for the first sensor of the first station
if (nrow(air_quality_data) > 0) {
  first_station_id <- air_quality_data$id[1]
  first_station_name <- air_quality_data$name[1]
  sensors_first <- tryCatch({
    download_sensors_for_location(api_key, first_station_id)
  }, error = function(e) {
    cat("Error fetching sensors for first station (ID:", first_station_id, "): ", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(sensors_first) && nrow(as.data.frame(sensors_first)) > 0) {
    first_sensor_id <- sensors_first$id[1]
    key <- paste0("station_", first_station_id, "_sensor_", first_sensor_id)
    if (!is.null(all_measurements[[key]])) {
      cat("Sample measurements for the first sensor of station ", first_station_name, ":\n")
      print(all_measurements[[key]])
    } else {
      cat("No measurements found for the first sensor of station: ", first_station_name, "\n")
    }
  } else {
    cat("No sensors found for the first station.\n")
  }
}
