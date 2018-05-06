# This script loads all of the data and has comments about where it is from

# It also writes a .csv/.rda of the boston flights data

# This script should not have to be run once we get the data that we want

library(data.table)
library(tidyverse)
library(lubridate)

# Flights -----------------------------------------------------------------
#download .zip file of flights data
#puts file in working directory as "flights.zip" due to the large size this zip file must be unzipped manually.
download.file(url = "https://packages.revolutionanalytics.com/datasets/AirOnTime87to12/AirOnTimeCSV.zip", destfile = "flights")


#once this file is unzipped the following code will merge the entirety of the flight data
#2002-2012
months <- c(paste0("0", 1:9), paste0("", 10:12))
get.full.year.flights <- function(year){
  year.data <- data.frame()
  year.str <- toString(year)
  for(m in 1:12){
    month.data <- fread(paste0("airOT",
                               year.str,
                               months[m],
                               ".csv"))
    year.data <- rbind(year.data, month.data)
  }
  return(year.data)
}

full.data <- data.frame()
for(i in 2002:2012){
  print(Sys.time())
  full.data <- rbind(get.full.year.flights(i), full.data)  
  print(i)
}

#filter for just flights originating in Boston
full.data.bos <- full.data %>%
  filter(ORIGIN == "BOS")

write.csv(full.data, "FlightsData0212.csv", row.names = FALSE)
write.csv(full.data.bos, "FlightsData0212BOS.csv", row.names = FALSE)


# Weather -----------------------------------------------------------------
#Weather data comes from https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND
#Read in BOS/NYC weather data from 2002-2012
weather.data <- fread("ATLBOSDETNYCweather0212.csv")
weather <- weather.data %>%
  mutate(DATE = as_date(DATE)) %>%
  mutate(SNOW = as.numeric(SNOW),
         PRCP = as.numeric(PRCP),
         SNWD = as.numeric(SNWD),
         TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX))
weather.atl <- weather %>%
  filter(NAME == "ATLANTA HARTSFIELD INTERNATIONAL AIRPORT, GA US")

weather.bos <- weather %>%
  filter(NAME == "BOSTON, MA US")

weather.det <- weather %>%
  filter(NAME == "DETROIT CITY AIRPORT, MI US")

weather.nyc <- weather %>%
  filter(NAME == "NY CITY CENTRAL PARK, NY US")

