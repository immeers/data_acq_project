library(dplyr)
library(lubridate)
library(stringr)
library(hms)

## American Airlines (ORD) Data
## Append CSVs
aa1 <- read.csv('AmericanAirlines.csv')
aa2 <- read.csv('AmericanAirlines2.csv')

aa1 <- aa1 %>%
  select(-X)
colnames(aa2) <- colnames(aa1)

aa <- bind_rows(aa1, aa2)

aa <- aa %>%
  filter(deptAirport != 'ORD')

## Get Wait Times
wait <- read.csv('waitTimes1.csv')
colnames(wait)[c(6,8)] <- c('US_Max_Wait_Time', 'Non_US_Max_Wait_Time')

wait$Date <- mdy(wait$Date)
wait <- wait %>%
  filter(Date >= mdy('08/01/2024') & Date <= mdy('08/07/2024'))

## Clean Date Columns
# Wait Times: Split Hour into Arrival/Dept Hour
wait[c('arrStart', 'arrEnd')] <- str_split_fixed(wait$Hour, ' ', 2)
wait$arrStart <- format(as.POSIXct(wait$arrStart, format="%H%M"), "%H:%M")
wait$arrEnd <- str_remove(wait$arrEnd, "- ")
wait$arrEnd <- format(as.POSIXct(wait$arrEnd, format="%H%M"), "%H:%M")

# Flight Times: Clean to hh:mm
aa$deptTime <- str_extract(aa$actualOutGate, "\\d{2}:\\d{2}")
aa$arrTime <- str_extract(aa$arrTime, "\\d{2}:\\d{2}")

# Merge dataframes
# if the arrival time is between arrStart, and arrEnd, take that row of data and merge
assign_hour <- function(row) {
  for (i in 1:nrow(wait)) {
    row_time <- as.POSIXct(row['arrTime'], format="%H:%M")
    start <- as.POSIXct(wait$arrStart[i], format="%H:%M")
    end <- as.POSIXct(wait$arrEnd[i], format="%H:%M")
    if ((row_time >= start) & (row_time <= end)) {
      # assign the rest of the columns
      row['US_Average_Wait_Time'] <- wait$US_Average_Wait_Time[i]
      row['US_Max_Wait_Time'] <- wait$US_Max_Wait_Time[i]
    }
  }
}

df <- apply(aa, 1, assign_hour)

