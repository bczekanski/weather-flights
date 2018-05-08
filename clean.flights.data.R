# This script cleans all of the data, merging files and introducing lag and different 
# weather data.

# There should be comments that make it clear where each of the data that is loaded comes from
# relative to the get.flights.data script

library(lubridate)
library(tidyverse)
library(data.table)

#MICHAEL
weather.data <- fread("~/Desktop/Statistics/weather-flights/ATLBOSDETNYCweather0212.csv")

#BEN
#flights <- fread("~/Downloads/FlightsData0212BOS.csv")

#MICHAEL
flights <- fread("~/Desktop/Statistics/Stats Final Project/FlightsData0212BOS.csv")

weather <- weather.data %>%
  mutate(DATE = as_date(DATE)) %>%
  mutate(SNOW = as.numeric(SNOW),
         PRCP = as.numeric(PRCP),
         SNWD = as.numeric(SNWD),
         TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX)) %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate_at(c("SNOW", "PRCP", "TMIN", "SNWD", "TMAX"),
            funs(lag1 = lag(., 1),
                 lag2 = lag(., 2),
                 lag3 = lag(., 3))) %>%
  mutate(WT01 = recode(WT01, "    1" = 1, .default = 0.0))

weather.data$WT01 = recode(weather.data$WT01,"    1" = 1, .default = 0.0)

weather.atl <- weather %>%
  filter(NAME == "ATLANTA HARTSFIELD INTERNATIONAL AIRPORT, GA US")

  weather.bos <- weather %>%
  filter(NAME == "BOSTON, MA US")

weather.nyc <- weather %>%
  filter(NAME == "NY CITY CENTRAL PARK, NY US") 

weather.det <- weather %>%
  filter(NAME == "DETROIT CITY AIRPORT, MI US")



# DAY or DAY_OF_MONTH
flights <- mutate(flights, DATE = as_date(paste(YEAR, MONTH, DAY_OF_MONTH, sep = "-")))

clean.flights <- weather.atl %>%
  left_join(weather.bos, by = "DATE", suffix = c(".atl", ".bos")) %>%
  left_join(weather.det, by = "DATE", suffix = c("", ".det")) %>%
  left_join(weather.nyc, by = "DATE", suffix = c("", ".nyc")) %>% 
  left_join(flights, ., by = "DATE")


            