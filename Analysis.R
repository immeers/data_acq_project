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
aa$arrDay <- as.Date(aa$arrDay)

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
## Make total mins column for both
aa <- aa %>%
  mutate(arrMinutes = as.numeric(sub(":.*", "", arrTime)) * 60 + as.numeric(sub(".*:", "", arrTime)))
wait <- wait %>%
  mutate(arrStartMins = as.numeric(sub(":.*", "", arrStart)) * 60 + as.numeric(sub(".*:", "", arrStart)),
         arrEndMins = as.numeric(sub(":.*", "", arrEnd)) * 60 + as.numeric(sub(".*:", "", arrEnd)))
# if the arrival time is between arrStart, and arrEnd, take that row of data and merge

result <- data.frame()
for (i in 1:nrow(aa)) {
  df1 <- aa[i,]
  matched_row <- wait %>%
    filter(Date == df1$arrDay, 
           arrStartMins <= df1$arrMinutes,
           arrEndMins >= df1$arrMinutes)
  #combined_row <- cbind(df1, matched_row[1, 5:15, drop = FALSE])
  combined_row <- df1
  if (nrow(matched_row) > 0) {
    for (j in 5:ncol(matched_row)) {
      col_name <- colnames(matched_row)[j]  
      combined_row[[col_name]] <- matched_row[1, j]
    }
  result <- rbind(result, combined_row)
  }
}

# Create day of week variable
result$DayOfWeek <- wday(result$arrDay, label = TRUE)

write.csv(result, "AA_wait_merged.csv", row.names = FALSE)

##################
# Run Linear Regression on Delay
wait_mod <- lm(Wait.Times.Average_Wait_Time ~ deptAirport + arrDay + arrMinutes,
               data = result)
summary(wait_mod)

us_data <- result %>%
  filter(deptCountry == 'US')
us_mod <- lm(US_Average_Wait_Time ~ deptAirport + arrDay + arrMinutes,
             data = us_data)
summary(us_mod)

non_us_data <- result %>%
  filter(deptCountry != 'US')
non_us_mod <- lm(Non_US_Average_Wait_Times ~ deptAirport + arrDay + arrMinutes,
             data = non_us_data)
summary(non_us_mod)




