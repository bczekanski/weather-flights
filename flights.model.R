# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights
#clean.flights <- sample_frac(clean_flights, 0.01)

set.seed(1)
training.set <- clean_flights %>%
  filter(DATE < as_date("2009-12-31")) 

small.training.set <-  training.set%>%
  sample_frac(0.01)
  #select(-c(STATION, NAME, DATE, YEAR, DAY_OF_MONTH, FL_DATE, TAIL_NUM, FL_NUM, ORIGIN_AIRPORT_ID, ORIGIN_STATE_ABR, DEST_AIRPORT_ID, DEST_STATE_ABR, DEP_TIME, 217:231, 233:238, 240:246))


test.set <- clean_flights %>%
  filter(DATE >= as_date("2009-12-31") & DATE < as_date("2012-01-01"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

#DO NOT TEST ON
#TO BE USED TO EVALUATE FINAL MODEL
secret.test <- clean.flights %>%
  filter(DATE >= as_date("2012-01-01"),
         DEST %in% unique(training.set$DEST),
         UNIQUE_CARRIER %in% unique(training.set$UNIQUE_CARRIER))

our.formula <- "factor(CANCELLED) ~ factor(MONTH) + DISTANCE"
print(Sys.time())

#Test Kappa >  Training Kappa because some cancelled flights don't have cancellation codes
training.model <- glm(our.formula,
                      family = "binomial",
                      data = training.set)
summary(training.model)

training.set$pred.glm <- stats::predict.glm(training.model,
                                        training.set)

glm.training.Kappa.val <- (fmsb::Kappa.test(table(training.set$pred.glm > 0.5, training.set$CANCELLED)))$Result$estimate

test.set$pred.glm <- stats::predict.glm(training.model,
                                    test.set,
                                    type = "response")

glm.test.Kappa.val <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred.glm > 0.5)))$Result$estimate


mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)

#Use ML to select variables ad get better predictive power
#Don't use SNOW.atl, TMAX.det, TMIN.det, TAVG.det
# factor(CANCELLED) ~ factor(MONTH) + factor(DAY_OF_WEEK) + DISTANCE +
#   TMAX + TMIN + WT01 + WT03 + SNOW + PRCP + AWND + WESD +
#   TMAX.atl + TMIN.atl + WT01.atl + WT03.atl + PRCP.atl + AWND.atl + WESD.atl +
#   TMAX.det + TMIN.det + WT01.det + WT03.det + PRCP.det + WESD.det +
#   TMAX.nyc + TMIN.nyc + WT01.nyc + WT03.nyc + PRCP.nyc + WESD.nyc +
#   TMAX_lag1 + TMIN_lag1 + SNOW_lag1 + PRCP_lag1 +
#   TMAX_lag1.atl + TMIN_lag1.atl + PRCP_lag1.atl +
#   TMAX_lag1.nyc + TMIN_lag1.nyc + PRCP_lag1.nyc +
#   PRCP_lag1.det


#BEST: tuneLength = 2, testKappa = 0.24
# factor(CANCELLED) ~ factor(MONTH) + factor(DAY_OF_WEEK) + 
#   TMAX + TMIN + WT01 + WT03 + SNOW + WESD +
#   TMAX.atl + TMIN.atl + WT01.atl + WT03.atl + PRCP.atl + AWND.atl + WESD.atl +
#   TMAX.det + TMIN.det + WT01.det + WT03.det + PRCP.det + WESD.det + 
#   TMAX.nyc + TMIN.nyc + WT03.nyc + WESD.nyc +
#   TMAX_lag1 + TMIN_lag1 + SNOW_lag1 + PRCP_lag1 +
#   TMAX_lag1.atl + TMIN_lag1.atl + PRCP_lag1.atl +
#   TMAX_lag1.nyc + TMIN_lag1.nyc + PRCP_lag1.nyc +
#   PRCP_lag1.det,
print(Sys.time())
alex <- train(factor(CANCELLED) ~ factor(DAY_OF_WEEK) + 
                TMAX + SNOW + WESD +
                TMIN.atl + WT01.atl + WT03.atl + PRCP.atl + WESD.atl +
                TMAX.det + TMIN.det + WT01.det + WT03.det + WESD.det + 
                TMIN.nyc + WT03.nyc + WESD.nyc +
                TMAX_lag1 + TMIN_lag1 + SNOW_lag1 +
                PRCP_lag1.atl +
                TMAX_lag1.nyc + TMIN_lag1.nyc + PRCP_lag1.nyc +
                PRCP_lag1.det,
              method = "LogitBoost",
              metric = "Kappa",
              tuneLength = 3,
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

#Random Forest
Sys.time()
alex <- train(factor(CANCELLED) ~ factor(MONTH) + factor(DAY_OF_WEEK) + DISTANCE +
                TMAX + TMIN + WT01 + WT03 + SNOW + PRCP + AWND + WESD +
                TMAX.atl + TMIN.atl + WT01.atl + WT03.atl + PRCP.atl + AWND.atl + WESD.atl +
                TMAX.det + TMIN.det + WT01.det + WT03.det + PRCP.det + WESD.det +
                TMAX.nyc + TMIN.nyc + WT01.nyc + WT03.nyc + PRCP.nyc + WESD.nyc +
                TMAX_lag1 + TMIN_lag1 + SNOW_lag1 + PRCP_lag1 +
                TMAX_lag1.atl + TMIN_lag1.atl + PRCP_lag1.atl +
                TMAX_lag1.nyc + TMIN_lag1.nyc + PRCP_lag1.nyc +
                PRCP_lag1.det,
              method = "rf",
              metric = "Kappa",
              data = sample_frac(small.training.set, 0.1))
Sys.time()
alex$results
alex$finalModel

training.set$pred.ml <- predict(alex, training.set)

training.Kappa.val.ml <- (fmsb::Kappa.test(table(training.set$pred.ml, training.set$CANCELLED)))$Result$estimate

small.training.set$pred.ml <- predict(alex, small.training.set)

small.training.Kappa.val.ml <- (fmsb::Kappa.test(table(small.training.set$pred.ml, small.training.set$CANCELLED)))$Result$estimate

test.set$pred.ml <- predict(alex,
                            test.set)

test.Kappa.val.ml <- (fmsb::Kappa.test(table(test.set$CANCELLED, test.set$pred.ml)))$Result$estimate

small.training.Kappa.val.ml
training.Kappa.val.ml
test.Kappa.val.ml

#Confusion Matrix for all 0's
table(test.set$CANCELLED, rep(0, nrow(test.set)))

#Confusion Matrix for Logistic Model
table(test.set$CANCELLED, test.set$pred.glm > 0.5)

#Confusion Matrix for LogitBoost
table(test.set$CANCELLED, (test.set$pred.ml == 1))
test.Kappa.val.ml

#Revaluating cut-off
