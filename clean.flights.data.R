# This script cleans all of the data, merging files and introducing lag and different 
# weather data.

# There should be comments that make it clear where each of the data that is loaded comes from
# relative to the get.flights.data script

weather.data <- fread("BOSNYCweather0212.csv")

library(lubridate)

weather <- weather.data %>%
  mutate(DATE = as_date(DATE)) %>%
  mutate(SNOW = as.numeric(SNOW),
         PRCP = as.numeric(PRCP),
         SNWD = as.numeric(SNWD),
         TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX))

weather.nyc <- weather %>%
  filter(NAME == "NY CITY CENTRAL PARK, NY US")

weather.bos <- weather %>%
  filter(NAME == "BOSTON, MA US")

flights <- mutate(flights, DATE = as_date(paste(YEAR, MONTH, DAY_OF_MONTH, sep = "-")))
# flights.bos <- flights %>%
#   filter(ORIGIN_AIRPORT == "BOS")

# cancel.model <- flights %>%
#   filter(ORIGIN_AIRPORT == "BOS") %>%
#   glm(factor(CANCELLED) ~ factor(AIRLINE) + WEATHER_DELAY + factor(DESTINATION_AIRPORT),
#       family = "binomial",
#       data = .)


clean.flights <- weather.bos %>%
  left_join(weather.nyc, by = "DATE", suffix = c(".bos", ".nyc")) %>% 
  left_join(flights, ., by = "DATE")
