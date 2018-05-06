setwd("~/Desktop/Statistics/Stats Final Project/AirOnTimeCSV")

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

flights <- full.data %>%
  filter(ORIGIN == "BOS")

setwd("~/Desktop/Statistics/Stats Final Project/")
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

# summary(cancel.model)


clean.flights <- weather.bos %>%
  left_join(weather.nyc, by = "DATE", suffix = c(".bos", ".nyc")) %>% 
  left_join(flights, ., by = "DATE")

training.set <- clean.flights %>%
  filter(DATE < as_date("2011-12-31"))

test.set <- clean.flights %>%
  filter(DATE >= as_date("2011-12-31"))
#samp <- sample(1:nrow(clean.flights), nrow(clean.flights)*0.6, replace = FALSE)
# training.set <- clean.flights[samp,]
# 
# test.set <- clean.flights[-samp,]

our_formula <- "DEP_DELAY ~ UNIQUE_CARRIER + UNIQUE_CARRIER*DEST + WEATHER_DELAY + SNOW.bos + SNOW.nyc + PRCP.bos + PRCP.nyc + TMIN.bos + TMIN.nyc + TMAX.bos + TMAX.nyc + MONTH"

training.model <- glm(CANCELLED ~ UNIQUE_CARRIER + SNOW.bos + SNOW.nyc +  PRCP.bos + PRCP.nyc + TMIN.bos + TMIN.nyc + TMAX.bos + TMAX.nyc + factor(MONTH),
                      family = "binomial",
                      data = training.set)
 summary(training.model)


test.set <-  test.set %>%
  mutate(pred = predict.lm(training.model,
                            .))

mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)


print(Sys.time())
alex <- train(factor(CANCELLED) ~ UNIQUE_CARRIER + DEST  + DEST*UNIQUE_CARRIER + SNOW.bos + SNOW.nyc + PRCP.bos +
                PRCP.nyc + TMIN.bos + TMIN.nyc + TMAX.bos + TMAX.nyc + factor(MONTH),
              method = "LogitBoost",
              metric = "Kappa",
              data = sample_frac(training.set, 0.01))
print(Sys.time())

alex$results

download.file(url = "https://packages.revolutionanalytics.com/datasets/AirOnTime87to12/AirOnTimeCSV.zip", destfile = "myzip")
a <- fread(zip.file.extract(file, zipname = "R.zip", unzip = getOption("unzip")))


