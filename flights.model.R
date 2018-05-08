# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights

#clean.flights <- sample_frac(clean_flights, 0.01)

training.set <- clean.flights %>%
  filter(DATE < as_date("2009-12-31")) %>%
  sample_frac(0.01)


test.set <- clean.flights %>%
  filter(DATE >= as_date("2009-12-31") & DATE < as_date("2012-01-01"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

#DO NOT TEST ON
#TO BE USED TO EVALUATE FINAL MODEL
secret.test <- clean.flights %>%
  filter(DATE >= as_date("2012-01-01"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

our.formula <- "factor(CANCELLED) ~ SNOW.bos + TMAX.bos + TMIN.bos + MONTH + DISTANCE + SNOW.nyc + TMAX.atl + TMIN.atl + TMAX.nyc + TMIN.nyc"
print(Sys.time())

#Test Kappa >  Training Kappa because some cancelled flights don't have cancellation codes
training.model <- glm(our.formula,
                      family = "binomial",
                      data = training.set)
summary(training.model)

training.set$pred.glm <- stats::predict.glm(training.model,
                                        training.set,
                                        type = "response")

glm.training.Kappa.val <- (fmsb::Kappa.test(table(training.set$pred.glm > 0.5, training.set$CANCELLED)))$Result$estimate

test.set$pred.glm <- stats::predict.glm(training.model,
                                    test.set,
                                    type = "response")

glm.test.Kappa.val <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred.glm > 0.5)))$Result$estimate


mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)

#Use ML to select variables ad get better predictive power
#SNOW.bos + TMAX.bos + TMIN.bos + DATE + DISTANCE + UNIQUE_CARRIER + SNOW.nyc + TMAX.atl + TMIN.atl + TMAX.nyc + TMIN.nyc
print(Sys.time())
alex <- train(factor(CANCELLED) ~ PRCP_lag1.atl,
              method = "LogitBoost",
              metric = "Kappa",
              tuneLength = 2,
              data = training.set)
print(Sys.time())

training.set$pred.ml <- predict(alex,
                             training.set)

training.Kappa.val.ml <- (fmsb::Kappa.test(table(training.set$pred.ml, training.set$CANCELLED)))$Result$estimate

test.set$pred.ml <- predict(alex,
                         test.set)

test.Kappa.val.ml <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred.ml)))$Result$estimate

alex$results

#Confusion Matrix for all 0's
table(test.set$CANCELLED, rep(0, nrow(test.set)))

#Confusion Matrix for Logistic Model
table(test.set$CANCELLED, test.set$pred.glm > 0.5)

#Confusion Matrix for LogitBoost
table(test.set$CANCELLED, test.set$pred.ml)

