# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights

clean.flights <- sample_frac(clean_flights, 0.01)

training.set <- clean.flights %>%
  filter(DATE < as_date("2009-12-31"))


test.set <- clean.flights %>%
  filter(DATE >= as_date("2009-12-31"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

our.formula <- "factor(CANCELLED) ~ factor(CANCELLATION_CODE)"

#Test Kappa >  Training Kappa because some cancelled flights don't have cancellation codes
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

#Use ML to select variables ad get better predictive power
#SNOW.bos + PRCP.bos + TMAX.bos + TMIN.bos + DATE + DISTANCE + UNIQUE_CARRIER
print(Sys.time())
alex <- train(factor(CANCELLED) ~ SNOW.bos + TMAX.bos + TMIN.bos + DATE + DISTANCE + UNIQUE_CARRIER + SNOW.nyc + TMAX.atl + TMIN.atl,
              method = "LogitBoost",
              metric = "Kappa",
              data = training.set)
print(Sys.time())

training.set$pred <- predict(alex,
                             data = training.set)

training.Kappa.val.ml <- (fmsb::Kappa.test(table(training.set$pred, training.set$CANCELLED)))$Result$estimate

test.set$pred <- predict(alex,
                         test.set)

test.Kappa.val.ml <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred)))$Result$estimate

alex$results
