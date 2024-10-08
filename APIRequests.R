library(httr)
library(jsonlite)
library(tidyr)

url <- "https://flight-info-api.p.rapidapi.com/status"
api_key = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e'
api_host = 'flight-info-api.p.rapidapi.com'

queryString <- list(
  version = "v2",
  ArrivalDateTime = "2024-08-01T00:00/2024-09-01T00:00",
  ArrivalAirport = "ORD",
  FlightType = "Scheduled",
  CodeType = "IATA",
  ServiceType = "Passenger"
)
flight_data <- data.frame()


connections <- function(stops, stops_list) {
  conn_list <- list()
  if (stops == 0) {
    conn_list <- append(conn_list, "None")
  }
  else {
    for (i in 1:stops) {
      conn_list <- append(conn_list, list(stops_list$station[i]))
    }
  }
  return (conn_list)
}


parse_resp <- function(response, flight_data){
  
  
  content <- content(response, "text")
  json <- fromJSON(content)
  list_data <- jsonlite::flatten(json$data) #flatten data frame, still with some nesting, will need to be cleaned
  # #particularly look at status details for scheduled departure/arrival (will be useful for delay)

  
  #Writing JSON resp to file incase something happens
  existing_data <- fromJSON("flightAPIJson.json")
  
  combined_data <- append(existing_data, content)
  
  write_json(combined_data, 'flightAPIJson.json')
  
  # 
  # #CLEAN AND APPEND EITHER TO DF OR CSV 
  # json <- fromJSON('flightAPIJson.json')

  for (i in 1:nrow(list_data)) {
    # data <- json$data[[i]]
    # new_row <- data.frame(
    #   airline = data$carrier$iata,
    #   deptAirport = data$departure$airport$iata,
    #   arrAirport = data$arrival$airport$iata,
    #   deptDay = data$departure$date$utc,
    #   deptTime = data$departure$time$utc,
    #   arrDay = data$arrival$date$utc,
    #   arrTime = data$arrival$time$utc,
    #   deptCountry = data$departure$country$code,
    #   arrCountry = data$arrival$country$code,
    #   flightNumber = data$flightNumber,
    #   numStops  = data$segmentInfo$numberOfStops,
    #   connections = connections(data)[[1]],
    #   miles = data$distance$accumulatedGreatCircleMiles
    # )

    data = list_data[i,]
    new_row1 <- data.frame(
      airline = data$carrier.iata,
      timeOfFlight = data$elapsedTime,
      deptAirport = data$departure.airport.iata,
      arrAirport = data$arrival.airport.iata,
      arrDay = data$arrival.date.local,
      arrTime = data$arrival.time.local,
      deptCountry = data$departure.country.code,
      flightNumber = data$flightNumber,
    
      estimatedOutGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$outGateVariation[[1]]), NA, list_data$statusDetails[[i]]$departure$estimatedTime$outGateVariation[[1]]),
      estimatedOutGate = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$outGate), NA, list_data$statusDetails[[i]]$departure$estimatedTime$outGate[[1]]),
      estimatedOffGround = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$offGround), NA, list_data$statusDetails[[i]]$departure$estimatedTime$offGround[[1]]),
      actualOutGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$outGateVariation), NA, list_data$statusDetails[[i]]$departure$actualTime$outGateVariation[[1]]),
      actualOutGate = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$outGate), NA, list_data$statusDetails[[i]]$departure$actualTime$outGate[[1]]), 
      actualOffGround = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$offGround), NA, list_data$statusDetails[[i]]$departure$actualTime$offGround[[1]]),
      
      estimatedInGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$inGateVariation), NA ,list_data$statusDetails[[i]]$arrival$estimatedTime$inGateVariation[[1]]),
      estimatedInGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$inGate), NA, list_data$statusDetails[[i]]$arrival$estimatedTime$inGate[[1]]),
      estimatedOnGround = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$onGround), NA, list_data$statusDetails[[i]]$arrival$estimatedTime$onGround[[1]]),
      actualInGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$inGateVariation), NA, list_data$statusDetails[[i]]$arrival$actualTime$inGateVariation[[1]]), 
      actualInGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$inGate), NA, list_data$statusDetails[[i]]$arrival$actualTime$inGate[[1]]),
      actualOnGround = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$onGround), NA, list_data$statusDetails[[i]]$arrival$actualTime$onGround[[1]]),
      
      arrTerminal = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTerminal), NA, list_data$statusDetails[[i]]$arrival$actualTerminal),
      arrGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$gate), NA, list_data$statusDetails[[i]]$arrival$gate),
      
      numStops  = data$segmentInfo.numberOfStops,
      connections = I(list(connections(list_data$segmentInfo.numberOfStops[[i]], list_data$segmentInfo.intermediateAirports.iata[[i]]))),
      miles = data$distance.accumulatedGreatCircleMiles
    )
    
    
    
    flight_data <- rbind(flight_data, new_row1)
  }
  write.csv(flight_data, 'request1.csv', row.names=FALSE)
  
  write.csv(flight_data, 'request2.csv', row.names=FALSE, append = TRUE)
  
  
  paging_next <- json$paging$`next`
  return(paging_next)
}

#This will run until the API limit is reached or reach end of pages
#Keep track of paging_next when reach limit so we can use for next account

#INITIAL REQUEST, but we already have it
response <- VERB("GET", url, query = queryString, add_headers('x-rapidapi-key' = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e', 'x-rapidapi-host' = 'flight-info-api.p.rapidapi.com'), content_type("application/octet-stream"))
counter  = 0

while(paging_next != ""){
  response <- VERB("GET", url = paging_next, add_headers('x-rapidapi-key' = '2a26d82d2fmsh3b29970cdca97d1p138b78jsn60a78272a81e', 'x-rapidapi-host' = 'flight-info-api.p.rapidapi.com'), content_type("application/octet-stream"))
  
  counter <- counter + 1
  
  if (status_code(response) != 200) {
    break #break out of while
    print("Error with request")
  }
  
  paging_next <- parse_resp(response, flight_data)
  
  #API Limit = 10 requests per minute
  Sys.sleep(7)
  
}



library(readr)
requests <- read_csv('request1.csv')

flight_data1 = data.frame()
for (j in 1:49){
  json2 <- fromJSON(json1[[j]])
  list_data <- jsonlite::flatten(json$data) #flatten data frame, still with some nesting, will need to be cleaned
  
  for (i in 1:nrow(list_data)) {
          
           data = list_data[i,]
           new_row1 <- data.frame(
             airline = data$carrier.iata,
             timeOfFlight = data$elapsedTime,
             deptAirport = data$departure.airport.iata,
             arrAirport = data$arrival.airport.iata,
             arrDay = data$arrival.date.local,
             arrTime = data$arrival.time.local,
             deptCountry = data$departure.country.code,
             flightNumber = data$flightNumber,
             
             estimatedOutGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$outGateVariation[[1]]), NA, list_data$statusDetails[[i]]$departure$estimatedTime$outGateVariation[[1]]),
             estimatedOutGate = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$outGate), NA, list_data$statusDetails[[i]]$departure$estimatedTime$outGate[[1]]),
             estimatedOffGround = ifelse(is.null(list_data$statusDetails[[i]]$departure$estimatedTime$offGround), NA, list_data$statusDetails[[i]]$departure$estimatedTime$offGround[[1]]),
             actualOutGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$outGateVariation), NA, list_data$statusDetails[[i]]$departure$actualTime$outGateVariation[[1]]),
             actualOutGate = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$outGate), NA, list_data$statusDetails[[i]]$departure$actualTime$outGate[[1]]), 
             actualOffGround = ifelse(is.null(list_data$statusDetails[[i]]$departure$actualTime$offGround), NA, list_data$statusDetails[[i]]$departure$actualTime$offGround[[1]]),
             
             estimatedInGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$inGateVariation), NA ,list_data$statusDetails[[i]]$arrival$estimatedTime$inGateVariation[[1]]),
             estimatedInGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$inGate), NA, list_data$statusDetails[[i]]$arrival$estimatedTime$inGate[[1]]),
             estimatedOnGround = ifelse(is.null(list_data$statusDetails[[i]]$arrival$estimatedTime$onGround), NA, list_data$statusDetails[[i]]$arrival$estimatedTime$onGround[[1]]),
             actualInGateVariation = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$inGateVariation), NA, list_data$statusDetails[[i]]$arrival$actualTime$inGateVariation[[1]]), 
             actualInGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$inGate), NA, list_data$statusDetails[[i]]$arrival$actualTime$inGate[[1]]),
             actualOnGround = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTime$onGround), NA, list_data$statusDetails[[i]]$arrival$actualTime$onGround[[1]]),
             
             arrTerminal = ifelse(is.null(list_data$statusDetails[[i]]$arrival$actualTerminal), NA, list_data$statusDetails[[i]]$arrival$actualTerminal),
             arrGate = ifelse(is.null(list_data$statusDetails[[i]]$arrival$gate), NA, list_data$statusDetails[[i]]$arrival$gate),
             
             numStops  = data$segmentInfo.numberOfStops,
             connections = I(list(connections(list_data$segmentInfo.numberOfStops[[i]], list_data$segmentInfo.intermediateAirports.iata[[i]]))),
             miles = data$distance.accumulatedGreatCircleMiles)
           
           
           
           
           flight_data1 <- rbind(flight_data1, new_row1)
           
          write.csv(flight_data1, 'request3.csv', row.names=FALSE)
}

}

load('APITry1.Rdata')
list_data <- jsonlite::flatten(json1$data)