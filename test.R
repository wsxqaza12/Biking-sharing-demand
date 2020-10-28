# Test
library(data.table)
library(dplyr)
library(plyr)
library(MASS)
library(lubridate)

test <- fread("data/te

test$datetime <- ymd_hms(test$datetime)
test$season  <- factor(test$season, labels = c("Spring", "Summer", "Fall", "Winter"))
test$weather <- factor(test$weather, labels = c("Clear or cloudy", "Mists",
                                                "Light rain or snow o "Heavy rain"))
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)


# test$times   <- strftime(test$datetime, format="%H:%M:%S")
test$hour <- hour(test$datetime)
# test$month <- lubridate::month(test$date, label=TRUE)
test$month <- lubridate::month(test$date)
test$day <- wday(test$datetime)
test$Weekday <- lubridate::wday(test$date, label=TRUE)
test$year <- lubridate::year(test$date)
test[,10:14]<-lapply(test[,10:14], as.factor)
####################################################################################
test$count <- exp(predict(model, test))

out <- test %>% dplyr::select(datetime, count)
out[out<0 ]=0

write.csv(out, file = "C:/Userscv.row.nSE)
