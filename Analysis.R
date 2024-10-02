library(dplyr)
library(lubridate)
library(stringr)
library(hms)
library(caret)
library(tidyr)
library(car)

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

# Create other needed variables
result$DayOfWeek <- wday(result$arrDay, label = TRUE)
result <- result %>% 
  mutate(delayed = ifelse(grepl('^-', actualInGateVariation), 1, 0))

to_numeric <- function(time_str) {
  if (is.na(time_str)) { return(NA)
  } else{
    negative <- ifelse(substr(time_str, 1, 1) == "-", 1, 0)
    time_str <- gsub("-", "", time_str)
    parts <- strsplit(time_str, ":")[[1]]
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    total_minutes <- hours * 60 + minutes
    if (negative == 1) {
      total_minutes <- -total_minutes
    }
    return(total_minutes)
  }
}
result$inGateVarMins <- sapply(result$actualInGateVariation, to_numeric)

write.csv(result, "AA_wait_merged.csv", row.names = FALSE)

##################
# Logistic Regression to Predict Delay (based on airport, day, and time of day)
result$outGateVarMins <- sapply(result$actualOutGateVariation, to_numeric)
result$outGateVarEst <- sapply(result$estimatedOutGateVariation, to_numeric)
result$deptEstTime <- str_extract(result$estimatedOutGate, "\\d{2}:\\d{2}")
no_na <- na.omit(result)
delay_mod <- glm(delayed ~ deptAirport + DayOfWeek + outGateVarEst, 
  #delayed ~ deptAirport + outGateVarMins + DayOfWeek, 
             data = no_na, 
             family = binomial)

predicted_prob <- predict(delay_mod, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
table(Actual = no_na$delayed, Predicted = predicted_class)
confusionMatrix(as.factor(predicted_class), as.factor(no_na$delayed))

test_data <- data.frame(deptAirport = c('LHR'), DayOfWeek = c('Wed'), outGateVarEst = c(-9))
predicted_prob_test <- predict(delay_mod, type = "response", newdata = test_data)
predicted_class_test <- ifelse(predicted_prob_test > 0.5, 1, 0)
predicted_class[1]



# Linear Models to Predict Wait Times
wait_mod <- lm(Wait.Times.Average_Wait_Time ~ DayOfWeek + arrMinutes + inGateVarMins,
               data = results)
summary(wait_mod)
crPlots(wait_mod)

us_data <- result %>%
  filter(deptCountry == 'US')
us_mod <- lm(US_Average_Wait_Time ~ DayOfWeek + arrMinutes + inGateVarMins,
             data = us_data)
summary(us_mod)
crPlots(us_mod)

non_us_data <- result %>%
  filter(deptCountry != 'US')
non_us_mod <- lm(Non_US_Average_Wait_Times ~ DayOfWeek + arrMinutes + inGateVarMins,
             data = non_us_data)
summary(non_us_mod)
crPlots(non_us_mod)

## Clustering to Compare Airports
by_airport <- results %>%
  group_by(deptAirport) %>%
  summarize(percent_delay = sum(delayed)/n(),
            avg_miles = mean(miles),
            avg_us_wait = mean(US_Average_Wait_Time),
            max_us_wait = max(US_Max_Wait_Time),
            avg_non_us_wait = mean(Non_US_Average_Wait_Times),
            max_non_us_wait = max(Non_US_Max_Wait_Time),
            overall_avg_wait = mean(Wait.Times.Average_Wait_Time),
            overall_max_wait = max(Wait.Times.Max_Wait_Time))
scaled_data <- scale(by_airport[-1])
set.seed(123456) 
num_clusters <- 4

fit_1 <- kmeans(scaled_data, centers = num_clusters, nstart = 25)
by_airport$cluster <- as.factor(fit_1$cluster)

clusters <- fit_1$cluster
centers <- fit_1$centers
cluster <- c(1: 4)
center_df <- data.frame(cluster, centers)
center_reshape <- gather(center_df, features, values, avg_miles:overall_max_wait)
g_heat_1 <- ggplot(data = center_reshape, # Set dataset
                   aes(x = features, y = cluster, fill = values)) + 
  scale_y_continuous(breaks = seq(1, 4, by = 1)) + 
  geom_tile() + coord_equal() +  
  theme_set(theme_bw(base_size = 22) ) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint =0, 
                       space = "Lab", 
                       na.value ="grey",
                       guide = "colourbar", 
                       aesthetics = "fill") +
  coord_flip()
g_heat_1


