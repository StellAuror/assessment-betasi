# pacman::p_load("ollamar")
# ollamar::pull("deepseek-r1:7b")
ollamar::list_models()


### Libraries
pacman::p_load(
  "httr2",
  "glue",
  "tidyverse",
  "jsonlite"
)

### Function to make parallel requests to classify sentences (key_factor)
parallelSentiment <- function(texts, model = "deepseek-r1:7b") {
  # Define the JSON schema
  format <- list(
    type = "object",
    properties = list(
      key_factor = list(type = "string")
    ),
    required = list("key_factor")
  )
  
  # Create httr2_request objects for each text with the appropriate prompt
  reqs <- lapply(texts, function(text) {
    prompt <- glue::glue("
      Your task is to identify the key factor mentioned in the given opinion about a hotel.
      Focus on specific aspects such as 'cleanliness', 'location', 'price', 'comfort', 'staff', 'amenities', 
      'view', 'noise', 'wifi', 'breakfast', 'service', 'bed', 'room', 'bathroom', 'parking', 'airconditioning', 
      'checkin', 'checkout', 'security', 'pool', 'spa', 'gym', 'furniture', 'decor', 'lighting', 'elevator', 
      'bar', 'restaurant', 'lobby', 'temperature', 'space', 'balcony', 'transportation', 'reception', 'minibar', 
      'shower', 'bedlinen', 'towels', 'heating', 'airflow', 'isolation', 'tv', 'fridge', 'pets', 'value', 
      'cleaning', 'hospitality', 'internet', or 'accessibility'.
    
      Respond with a single word that best represents the main factor highlighted in the opinion.
    
      {text}
    ")
    
    ollamar::generate(
      model = model,
      prompt = prompt,
      output = "req",
      format = format  
    )
  })
  
  # Perform parallel requests
  responses <- req_perform_parallel(reqs)
  
  # Process responses
  sentiments <- sapply(responses, function(resp) {
    result <- ollamar::resp_process(resp, "text")
    # Parse JSON and extract the 'key_factor' field
    json_result <- jsonlite::fromJSON(result)
    json_result$key_factor
  })
  
  return(sentiments)
}


### Data
# Loading data
dfOpions <-
  readr::read_csv("data/GoogleHotelData.csv") 

# Processing data
ai_opions <- 
  dfOpions$reviews.text |>
  parallelSentiment() 

# Displaying result
dfOpions$reviews.text.aievaluated <-  ai_opions 

readr::write_csv(dfOpions, "data/GoogleHotelData.csv")

