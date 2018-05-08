# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights

#clean.flights <- sample_frac(clean_flights, 0.01)

training.set <- clean.flights %>%
  filter(DATE < as_date("2009-12-31")) %>%
  sample_frac(0.05)


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
#Don't use SNOW.atl, TMAX.det, TMIN.det, TAVG.det
#WT03 + WT03.nyc + WT03.atl + WT03.det +
# WV03 + WV03.nyc + WV03.atl + WV03.det + MONTH + DAY_OF_WEEK + DISTANCE + SNOW + 
#   TMAX + TMIN + TMAX.atl + TMIN.atl + PRCP + PRCP.nyc + PRCP.det + PRCP.atl
print(Sys.time())
alex <- train(factor(CANCELLED) ~ MONTH + DAY_OF_WEEK + DISTANCE +
                TMAX.atl + TMIN.atl + PRCP.nyc + PRCP.det + PRCP.atl +
                WT03 + WT03.nyc + WT03.atl + WT03.det + WV03 + WV03.nyc + WV03.atl + WV03.det +
                WT01 + WT01.nyc + WT01.atl + WT01.det + WT04 + WT04 + SNOW + SNOW_lag1 + SNOW_lag2 + SNOW_lag2 + 
                PRCP + PRCP_lag1  + PRCP_lag2 + PRCP_lag3 + TMIN + TMIN_lag1 + TMIN_lag2 + TMIN_lag3 +
                TMAX + TMAX_lag1 + TMAX_lag2 + TMAX_lag3 + WESD + WT02 + WT04 + WT05 + WT06 + WT07 + WT08 + WT09 +
                WT10 + WT11 + WT13 + WT14 + WT15 + WT16 + WT17 + WT18 + WT19 + WT21 + WT22 + DEST_STATE_ABR + DEST_STATE_ABR*PRCP +
                DEST_STATE_ABR*PRCP.nyc + DEST_STATE_ABR*PRCP.atl + DEST_STATE_ABR*PRCP.det,
              method = "LogitBoost",
              metric = "Kappa",
              data = training.set)

print(Sys.time())
alex$results
alex$finalModel

training.set$pred.ml <- predict(alex,
                             training.set)

training.Kappa.val.ml <- (fmsb::Kappa.test(table(training.set$pred.ml, training.set$CANCELLED)))$Result$estimate

test.set$pred.ml <- predict(alex,
                         test.set)

test.Kappa.val.ml <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred.ml)))$Result$estimate

#Confusion Matrix for all 0's
table(test.set$CANCELLED, rep(0, nrow(test.set)))

#Confusion Matrix for Logistic Model
table(test.set$CANCELLED, test.set$pred.glm > 0.5)

#Confusion Matrix for LogitBoost
table(test.set$CANCELLED, test.set$pred.ml)

