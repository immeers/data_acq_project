library(httr)
library(jsonlite)

url <- "https://flight-info-api.p.rapidapi.com/status"
api_key = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e'
api_host = 'flight-info-api.p.rapidapi.com'

queryString <- list(
  version = "v2",
  ArrivalDateTime = "2024-08-01T00:00/2024-09-01T00:00",
  DepartureAirport = "ORD",
  FlightType = "Scheduled",
  CodeType = "IATA",
  ServiceType = "Passenger"
)
#DOn't run this as the first request is already stored in flightAPIJson.json
#response <- VERB("GET", url, query = queryString, add_headers('x-rapidapi-key' = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e', 'x-rapidapi-host' = 'flight-info-api.p.rapidapi.com'), content_type("application/octet-stream"))

parse_resp <- function(response){
  
  
  content <- content(response, "text")
  json <- fromJSON(content)
  list_data <- jsonlite::flatten(json$data) #flatten data frame, still with some nesting, will need to be cleaned
  #particularly look at status details for scheduled departure/arrival (will be useful for delay)
  
  #Just run this to get inital json
  json <- read_json(flightAPIJson.json)
  
  
  #CLEAN AND APPEND EITHER TO DF OR CSV 
  
  
  paging_next <- json$paging$`next`
  return(paging_next)
}


#This will run until the API limit is reached or reach end of pages
#Keep track of paging_next when reach limit so we can use for next account

while(paging_next != ""){
  response <- VERB("GET", url = paging_next, add_headers('x-rapidapi-key' = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e', 'x-rapidapi-host' = 'flight-info-api.p.rapidapi.com'), content_type("application/octet-stream"))
  
  if (status_code(response) != 200) {
    break #break out of while
    print("Error with request")
  }
  
  paging_next <- parse_resp(response)
  
  #CHECK API MIN LIMIT AND ADD SLEEP IF NEED
}