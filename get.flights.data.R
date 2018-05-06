# This script loads all of the data and has comments about where it is from

# It also writes a .csv/.rda of the boston flights data

# This script should not have to be run once we get the data that we want

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