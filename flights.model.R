# This script does the model with comments
# It loads data from the clean.flights.data.R script

library(caret)
library(tidyverse)

clean_flights <- clean.flights

clean.flights <- sample_frac(clean_flights, 0.1)

training.set <- clean.flights %>%
  filter(DATE < as_date("2009-12-31"))

test.set <- clean.flights %>%
  filter(DATE >= as_date("2009-12-31"))

our.formula <- "CANCELLED ~ SNOW.bos"

training.model <- glm(our.formula,
                      family = "binomial",
                      data = training.set)
summary(training.model)


test.set <-  test.set %>%
  mutate(pred = predict.lm(training.model,
                           .))

mse.training.model <- mean((test.set$pred - test.set$CANCELLED)^2)



alex <- train(factor(CANCELLED) ~ SNOW.bos,
              method = "LogitBoost",
              metric = "Kappa",
              data = training.set)
print(Sys.time())

alex$results