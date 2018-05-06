# This script does the model with comments
# It loads data from the clean.flights.data.R script

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