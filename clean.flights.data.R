# This script cleans all of the data, merging files and introducing lag and different 
# weather data.

# There should be comments that make it clear where each of the data that is loaded comes from
# relative to the get.flights.data script

library(lubridate)
library(tidyverse)
library(data.table)

weather.data <- fread("ATLBOSDETNYCweather0212.csv")

#BEN
flights <- fread("~/Downloads/FlightsData0212BOS.csv")

#MICHAEL
# flights <- fread("~/Desktop/Statistics/Stats Final Project/FlightsData0212BOS.csv")

vars_to_recode <- tidyselect::vars_select(colnames(weather.data), starts_with("W"))

weather <- weather.data %>%
  mutate(DATE = as_date(DATE)) %>%
  mutate(SNOW = as.numeric(SNOW),
         PRCP = as.numeric(PRCP),
         SNWD = as.numeric(SNWD),
         TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX),
         TAVG = as.numeric(TAVG)) %>%
  mutate(PRCP = ifelse(is.na(PRCP), 0, PRCP)) %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate_at(c("SNOW", "PRCP", "TMIN", "SNWD", "TMAX"),
            funs(lag1 = lag(., 1),
                 lag2 = lag(., 2),
                 lag3 = lag(., 3))) %>%
mutate_at(vars_to_recode, funs(recode(., "    1" = 1, .default = 0.0))) %>%
  dplyr::select(-starts_with("WD")) %>%
  # dplyr::select(-starts_with("WS")) %>%
  filter(DATE >= lubridate::as_date("2003-09-01"),
         DATE != lubridate::as_date("2006-07-10")) %>%
  mutate(PRCP.TMIN.l32 = PRCP*(TMIN < 32)) %>%
  mutate(PRCP.TMAX.l32 = PRCP*(TMAX < 32))

  

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

clean.flights <- weather.bos %>%
  left_join(weather.atl, by = "DATE", suffix = c("", ".atl")) %>%
  left_join(weather.det, by = "DATE", suffix = c("", ".det")) %>%
  left_join(weather.nyc, by = "DATE", suffix = c("", ".nyc")) %>% 
  left_join(., flights, by = "DATE") %>%
  mutate(NY = factor(DEST_STATE_ABR == "NY"),
         MI = factor(DEST_STATE_ABR == "MI"),
         GA = factor(DEST_STATE_ABR == "GA"))

            