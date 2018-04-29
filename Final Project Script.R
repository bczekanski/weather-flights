flights <- fread("flights.csv")
weather.2015 <- fread("BOSweather2015.csv")
library(lubridate)

weather <- weather.2015 %>%
  mutate(DATE = as_date(DATE)) %>%
  filter(STATION == "USW00014739") %>%
  dplyr::select("PRCP", "SNOW", "DATE", "TAVG") %>%
  mutate(SNOW = as.numeric(SNOW),
         PRCP = as.numeric(PRCP),
         TAVG = as.numeric(TAVG))

flights <- mutate(flights, DATE = as_date(paste(YEAR, MONTH, DAY, sep = "-")))
flights.bos <- flights %>%
  filter(ORIGIN_AIRPORT == "BOS")

# cancel.model <- flights %>%
#   filter(ORIGIN_AIRPORT == "BOS") %>%
#   glm(factor(CANCELLED) ~ factor(AIRLINE) + WEATHER_DELAY + factor(DESTINATION_AIRPORT),
#       family = "binomial",
#       data = .)

# summary(cancel.model)


clean.flights.bos <- right_join(flights.bos, weather, by = "DATE")
# training.set <- flights.bos %>%
#   filter(DATE < as_date("2015-08-14"))
# 
# test.set <- flights.bos %>%
#   filter(DATE >= as_date("2015-08-14"))
samp <- sample(1:nrow(clean.flights.bos), nrow(clean.flights.bos)*0.6, replace = FALSE)
training.set <- clean.flights.bos[samp,]

test.set <- clean.flights.bos[-samp,]

training.model <- glm(factor(CANCELLED) ~ WEATHER_DELAY + SNOW + PRCP + TAVG,
    family = "binomial",
    data = training.set)
summary(training.model)

test.set <-  test.set %>%
  mutate(pred = predict.glm(training.model,
                           .,
                           type = "response"))

mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)
