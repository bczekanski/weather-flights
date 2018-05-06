# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights

clean.flights <- sample_frac(clean_flights, 0.2)

training.set <- clean.flights %>%
  filter(DATE < as_date("2009-12-31"))

test.set <- clean.flights %>%
  filter(DATE >= as_date("2009-12-31"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

our.formula <- "factor(CANCELLED) ~ SNOW.bos + SNOW_lag1.bos + SNOW_lag2.bos + SNOW_lag3.bos +
                SNOW.nyc + SNOW_lag1.nyc + SNOW_lag2.nyc + SNOW_lag3.nyc + TMIN.bos + TMAX.bos + factor(MONTH)
                + TMIN.nyc + TMAX.nyc + TMIN.atl + TMAX.atl + PRCP.bos + DISTANCE + DEST + UNIQUE_CARRIER +
                DEST*UNIQUE_CARRIER + factor(CANCELLATION_CODE)"

training.model <- glm(our.formula,
                      family = "binomial",
                      data = training.set)
summary(training.model)

training.set$pred <- stats::predict.glm(training.model,
                                        training.set,
                                        type = "response")

training.Kappa.val <- (fmsb::Kappa.test(table(training.set$pred > 0.5, training.set$CANCELLED)))$Result$estimate

test.set$pred <- stats::predict.glm(training.model,
                           test.set,
                           type = "response")

Kappa.val <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred > 0.5)))$Result$estimate


mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)



alex <- train(factor(CANCELLED) ~ SNOW.bos,
              method = "LogitBoost",
              metric = "Kappa",
              data = training.set)
print(Sys.time())

alex$results
