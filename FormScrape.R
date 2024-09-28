library(httr) # httr is organised around the six most common http verbs: GET(), PATCH(), POST(), HEAD(), PUT(), and DELETE().
library(rvest) # Easily Harvest (Scrape) Web Pages
library(tidyverse)
library(stringr)
library(jsonlite)

#Don't actually need this anymore as focusing on one airport
prep_ids <- function() {
#got from html of website
airport_ids <-  '<select name="Id" id="Airport" fdprocessedid="tufdsr">
  <option value="">--- Select Airport/Terminal ---</option>
  <option value="GUM">Antonio B. Won Pat International Airport</option>
  <option value="A810">---GUM Terminal: Main Terminal</option>
  <option value="ATL">Atlanta Hartsfield-Jackson Intl Airport</option>
  <option value="A171">---ATL Terminal: Concourse E</option>
  <option value="A174">---ATL Terminal: Concourse F</option>
  <option value="ATL">Atlanta Hartsfield-Jackson Intl Airport</option>
  <option value="A171">---ATL Terminal: Concourse E</option>
  <option value="A174">---ATL Terminal: Concourse F</option>
  <option value="AUS">Austin-Bergstrom International Airport</option>
  <option value="A555">---AUS Terminal: Main</option>
  <option value="BWI">Baltimore/Washington International Thurgood Marshall Airport</option>
  <option value="A131">---BWI Terminal: International Arrivals</option>
  <option value="BOS">Boston Logan International Airport</option>
  <option value="A041">---BOS Terminal: Terminal E</option>
  <option value="CLT">Charlotte/Douglas International Airport</option>
  <option value="A152">---CLT Terminal: Main</option>
  <option value="MDW">Chicago Midway International Airport</option>
  <option value="A398">---MDW Terminal: Main Terminal</option>
  <option value="ORD">Chicago O\'Hare International Airport</option>
  <option value="A392">---ORD Terminal: Terminal 5</option>
  <option value="CVG">Cincinnati/Northern Kentucky International Airport</option>
                                                    <option value="A412">---CVG Terminal: Concourse B</option>
                                                    <option value="DFW">Dallas/Fort Worth Intl Airport</option>
                                                    <option value="A557">---DFW Terminal: Terminal D</option>
                                                    <option value="DEN">Denver International Airport</option>
                                                    <option value="A337">---DEN Terminal: International</option>
                                                    <option value="DTW">Detroit Wayne County</option>
                                                    <option value="A385">---DTW Terminal: McNamara Terminal</option>
                                                    <option value="A381">---DTW Terminal: Warren C Evans Terminal</option>
                                                    <option value="DTW">Detroit Wayne County</option>
                                                    <option value="A385">---DTW Terminal: McNamara Terminal</option>
                                                    <option value="A381">---DTW Terminal: Warren C Evans Terminal</option>
                                                    <option value="FLL">Fort Lauderdale-Hollywood International Airport</option>
                                                    <option value="A52L">---FLL Terminal: Terminal 1</option>
                                                    <option value="A522">---FLL Terminal: Terminal 4</option>
                                                    <option value="FLL">Fort Lauderdale-Hollywood International Airport</option>
                                                    <option value="A52L">---FLL Terminal: Terminal 1</option>
                                                    <option value="A522">---FLL Terminal: Terminal 4</option>
                                                    <option value="FAT">Fresno Yosemite International Airport</option>
                                                    <option value="A287">---FAT Terminal: Main</option>
                                                    <option value="HNL">Honolulu International Airport</option>
                                                    <option value="A321">---HNL Terminal: Main Overseas Terminal</option>
                                                    <option value="IAH">Houston George Bush Intercontinental Airport</option>
                                                    <option value="A534">---IAH Terminal: IAB</option>
                                                    <option value="SNA">John Wayne Airport</option>
                                                    <option value="A27H">---SNA Terminal: Terminal C</option>
                                                    <option value="ONT">LA/Ontario International Airport</option>
                                                    <option value="A275">---ONT Terminal: International Terminal</option>
                                                    <option value="STL">Lambert-St. Louis International Airport</option>
                                                    <option value="A423">---STL Terminal: Main</option>
                                                    <option value="LAX">Los Angeles International Airport</option>
                                                    <option value="A272">---LAX Terminal: Satellite 2</option>
                                                    <option value="A271">---LAX Terminal: Satellite 5</option>
                                                    <option value="A278">---LAX Terminal: Satellite 7</option>
                                                    <option value="A279">---LAX Terminal: Terminal 4</option>
                                                    <option value="A273">---LAX Terminal: Tom Bradley International Terminal</option>
                                                    <option value="LAX">Los Angeles International Airport</option>
                                                    <option value="A272">---LAX Terminal: Satellite 2</option>
                                                    <option value="A271">---LAX Terminal: Satellite 5</option>
                                                    <option value="A278">---LAX Terminal: Satellite 7</option>
                                                    <option value="A279">---LAX Terminal: Terminal 4</option>
                                                    <option value="A273">---LAX Terminal: Tom Bradley International Terminal</option>
                                                    <option value="LAX">Los Angeles International Airport</option>
                                                    <option value="A272">---LAX Terminal: Satellite 2</option>
                                                    <option value="A271">---LAX Terminal: Satellite 5</option>
                                                    <option value="A278">---LAX Terminal: Satellite 7</option>
                                                    <option value="A279">---LAX Terminal: Terminal 4</option>
                                                    <option value="A273">---LAX Terminal: Tom Bradley International Terminal</option>
                                                    <option value="LAX">Los Angeles International Airport</option>
                                                    <option value="A272">---LAX Terminal: Satellite 2</option>
                                                    <option value="A271">---LAX Terminal: Satellite 5</option>
                                                    <option value="A278">---LAX Terminal: Satellite 7</option>
                                                    <option value="A279">---LAX Terminal: Terminal 4</option>
                                                    <option value="A273">---LAX Terminal: Tom Bradley International Terminal</option>
                                                    <option value="LAX">Los Angeles International Airport</option>
                                                    <option value="A272">---LAX Terminal: Satellite 2</option>
                                                    <option value="A271">---LAX Terminal: Satellite 5</option>
                                                    <option value="A278">---LAX Terminal: Satellite 7</option>
                                                    <option value="A279">---LAX Terminal: Terminal 4</option>
                                                    <option value="A273">---LAX Terminal: Tom Bradley International Terminal</option>
                                                    <option value="SJU">Luis Munoz Marin International Airport</option>
                                                    <option value="A492">---SJU Terminal: San Juan AA</option>
                                                    <option value="LAS">McCarran International Airport</option>
                                                    <option value="A274">---LAS Terminal: Terminal 3</option>
                                                    <option value="OAK">Metropolitan Oakland International Airport</option>
                                                    <option value="A283">---OAK Terminal: Main</option>
                                                    <option value="MIA">Miami International Airport</option>
                                                    <option value="A524">---MIA Terminal: Central Terminal</option>
                                                    <option value="A520">---MIA Terminal: North Terminal</option>
                                                    <option value="A52G">---MIA Terminal: South Terminal</option>
                                                    <option value="MIA">Miami International Airport</option>
                                                    <option value="A524">---MIA Terminal: Central Terminal</option>
                                                    <option value="A520">---MIA Terminal: North Terminal</option>
                                                    <option value="A52G">---MIA Terminal: South Terminal</option>
                                                    <option value="MIA">Miami International Airport</option>
                                                    <option value="A524">---MIA Terminal: Central Terminal</option>
                                                    <option value="A520">---MIA Terminal: North Terminal</option>
                                                    <option value="A52G">---MIA Terminal: South Terminal</option>
                                                    <option value="MSP">Minneapolis-St. Paul International Airport (Wold-Chamberlain Field)</option>
                                                    <option value="A354">---MSP Terminal: Terminal 1  Lindbergh</option>
                                                    <option value="A351">---MSP Terminal: Terminal 2  Humphrey</option>
                                                    <option value="MSP">Minneapolis-St. Paul International Airport (Wold-Chamberlain Field)</option>
                                                    <option value="A354">---MSP Terminal: Terminal 1  Lindbergh</option>
                                                    <option value="A351">---MSP Terminal: Terminal 2  Humphrey</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="JFK">New York J F Kennedy International Airport</option>
                                                    <option value="A475">---JFK Terminal: Delta</option>
                                                    <option value="A477">---JFK Terminal: Terminal 1</option>
                                                    <option value="A471">---JFK Terminal: Terminal 4 (IAT)</option>
                                                    <option value="A470">---JFK Terminal: Terminal 5 (Jet Blue)</option>
                                                    <option value="A472">---JFK Terminal: Terminal 7 (British)</option>
                                                    <option value="A473">---JFK Terminal: Terminal 8 (American)</option>
                                                    <option value="EWR">Newark Liberty International Airport</option>
                                                    <option value="A103">---EWR Terminal: Terminal B</option>
                                                    <option value="A105">---EWR Terminal: Terminal C</option>
                                                    <option value="EWR">Newark Liberty International Airport</option>
                                                    <option value="A103">---EWR Terminal: Terminal B</option>
                                                    <option value="A105">---EWR Terminal: Terminal C</option>
                                                    <option value="SJC">Norman Y. Mineta San Jose International Airport</option>
                                                    <option value="A284">---SJC Terminal: Main</option>
                                                    <option value="MCO">Orlando International Airport</option>
                                                    <option value="A185">---MCO Terminal: Airside 1</option>
                                                    <option value="A182">---MCO Terminal: Airside 4</option>
                                                    <option value="MCO">Orlando International Airport</option>
                                                    <option value="A185">---MCO Terminal: Airside 1</option>
                                                    <option value="A182">---MCO Terminal: Airside 4</option>
                                                    <option value="SFB">Orlando Sanford Airport</option>
                                                    <option value="A186">---SFB Terminal: Terminal A</option>
                                                    <option value="PBI">Palm Beach International Airport</option>
                                                    <option value="A523">---PBI Terminal: Main</option>
                                                    <option value="PHL">Philadelphia International Airport</option>
                                                    <option value="A111">---PHL Terminal: Terminal A</option>
                                                    <option value="PHX">Phoenix Sky Harbor International Airport</option>
                                                    <option value="A263">---PHX Terminal: Main</option>
                                                    <option value="PDX">Portland International Airport</option>
                                                    <option value="A291">---PDX Terminal: Main</option>
                                                    <option value="RDU">Raleigh-Durham International Airport</option>
                                                    <option value="A153">---RDU Terminal: Terminal 2</option>
                                                    <option value="SMF">Sacramento International Airport</option>
                                                    <option value="A285">---SMF Terminal: Main Terminal</option>
                                                    <option value="SPN">Saipan International Airport (Francisco C. Ada)</option>
                                                    <option value="A328">---SPN Terminal: </option>
                                                    <option value="SLC">Salt Lake City International Airport</option>
                                                    <option value="A33J">---SLC Terminal: Main</option>
                                                    <option value="A336">---SLC Terminal: Main(OLD)</option>
                                                    <option value="SLC">Salt Lake City International Airport</option>
                                                    <option value="A33J">---SLC Terminal: Main</option>
                                                    <option value="A336">---SLC Terminal: Main(OLD)</option>
                                                    <option value="SAT">San Antonio International Airport</option>
                                                    <option value="A554">---SAT Terminal: Main</option>
                                                    <option value="SAN">San Diego International Airport</option>
                                                    <option value="A251">---SAN Terminal: Main</option>
                                                    <option value="SFO">San Francisco International Airport</option>
                                                    <option value="A286">---SFO Terminal: Terminal A</option>
                                                    <option value="A281">---SFO Terminal: Terminal G</option>
                                                    <option value="SFO">San Francisco International Airport</option>
                                                    <option value="A286">---SFO Terminal: Terminal A</option>
                                                    <option value="A281">---SFO Terminal: Terminal G</option>
                                                    <option value="SEA">Seattle-Tacoma International Airport</option>
                                                    <option value="A301">---SEA Terminal: South Satellite</option>
                                                    <option value="TPA">Tampa International Airport</option>
                                                    <option value="A181">---TPA Terminal: Main</option>
                                                    <option value="PVD">Theodore Francis Green State Airport</option>
                                                    <option value="A051">---PVD Terminal: International Arrivals</option>
                                                    <option value="IAD">Washington Dulles International Airport</option>
                                                    <option value="A541">---IAD Terminal: International A</option>
                                                    <option value="A542">---IAD Terminal: Satellite C</option>
                                                    <option value="IAD">Washington Dulles International Airport</option>
                                                    <option value="A541">---IAD Terminal: International A</option>
                                                    <option value="A542">---IAD Terminal: Satellite C</option>'
out_df <- data.frame()

list_ids <- str_extract_all(airport_ids, '"[A-Z0-9]+">.+<')


for(i in 1:length(list_ids[[1]])){
  split_values <- strsplit(list_ids[[1]][i], '">')
  out_df <- rbind(out_df, data.frame(id = substring(split_values[[1]][1], 2), name = substr(split_values[[1]][2], 1, nchar(split_values[[1]][2]) - 1)))
}
out_df #ids and names of all airports
length(grep("[A-Z]{3}", unique(out_df$id)))
}


ord_id <- 'ORD'
t5_id <- 'A392'
#form request
awt_session <- session("https://awt.cbp.gov/")
form <- html_form(awt_session)[[1]]
form_filled <- html_form_set(form, Id = 'ORD', FromDate = '08/01/2024', ToDate = '09/01/2024')
answer <- session_submit(awt_session, form_filled)
answer
tbl <- data.frame(html_table(answer)[[1]])
for (i in (5:20)){
  colnames(tbl)[i] <- paste0(paste0(tbl[1, i], " "), gsub("\\s+", "_", tbl[2, i]))
}

colnames(tbl)[c(5,7)] <- c('US_Average_Wait_Time', 'Non_US_Average_Wait_Times')
tbl <- tbl %>% select(-All.12) %>% slice(-c(1, 2))
write.csv(tbl, 'waitTimes1.csv', row.names=FALSE)




