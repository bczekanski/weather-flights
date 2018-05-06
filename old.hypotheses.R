library(tidyverse)

download.file(url = "https://packages.revolutionanalytics.com/datasets/AirOnTime87to12/AirOnTimeCSV.zip", destfile = "myzip")
a <- fread(zip.file.extract("airOT198710.csv", zipname = "myzip.zip", unzip = getOption("unzip")))


# Load Libraries ----------------------------------------------------------
library(data.table)
library(lubridate)
library(tidyverse)


# Merge Flights Data ------------------------------------------------------
months <- c(paste0("0", 1:9), paste0("", 10:12))
get.full.year.flights <- function(year){
  year.data <- data.frame()
  year.str <- toString(year)
  for(m in 1:12){
    print(m)
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

setwd("~/Desktop/Statistics/Stats Final Project")
write.csv(full.data, "FlightsData0212.csv", row.names = FALSE)

full.data.bos <- full.data %>%
  filter(ORIGIN == "BOS")

write.csv(full.data.bos, "FlightsData0212BOS.csv", row.names = FALSE)




# Clean Weather Data ------------------------------------------------------

setwd("~/Desktop/Statistics/Stats Final Project/")
weather.data <- fread("BOSNYCweather0212.csv")


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
# Functions ---------------------------------------------------------------

bootstrap.chi.sq.once <- function(col1, col2, iter){
  new.col2 <- sample(col2)
  new.table <- table(col1, new.col2)
  return(data.frame(Stat = chisq.test(new.table)$statistic,
                    Iteration = iter))
}


# Hypothesis Testing Flight Data ------------------------------------------
original.dest.chi.val <- chisq.test(table(full.data.bos$DEST, full.data.bos$CANCELLED))$statistic
sum(chisq.test(table(full.data.bos$DEST, full.data.bos$CANCELLED))$expected < 5)

original.dest <- data.frame(DEST = full.data.bos$DEST,
                            CANCELLED = full.data.bos$CANCELLED)
new.dest.data <- original.dest
chisq.dest.vals <- NULL
for(i in 1:10000){
  print(i)
  new.dest.data$CANCELLED <- sample(original.dest$CANCELLED, replace = FALSE) #Permute the values
  dest.table <- table(new.dest.data$DEST, new.dest.data$CANCELLED)
  chisq.dest.vals[i] <- chisq.test(table(dest.table))$statistic
}

registerDoMC(cores = 3)
dest.chisq.df <- foreach(i = 1:10000, .combine = rbind) %dopar% {
  bootstrap.chi.sq.once(full.data.bos$DEST, full.data.bos$CANCELLED, i)
}
registerDoMC(cores = 1)



mean(chisq.dest.vals > original.dest.chi.val)


# Hypothesis Testing Weather Data -----------------------------------------
weather.data <- fread("BOSNYCweather0212.csv")
