library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(GGally)
bike <- fread("data/Train.csv")

##### PART1. 處理時間問題####
bike$datetime <- ymd_hms(bike$datetime)
bike$season  <- factor(bike$season, labels = c("Spring", "Summer", "Fall", "Winter"))
bike$weather <- factor(bike$weather, labels = c("Clear or cloudy", "Mists",
                                                "Light rain or snow", "Heavy rain"))
bike$holiday <- as.factor(bike$holiday)
bike$workingday <- as.factor(bike$workingday)


# bike$times   <- strftime(bike$datetime, format="%H:%M:%S")
bike$hour <- hour(bike$datetime)
# bike$month <- lubridate::month(bike$date, label=TRUE)
bike$month <- lubridate::month(bike$date)
bike$day <- wday(bike$datetime)
bike$Weekday <- lubridate::wday(bike$date, label=TRUE)
bike$year <- lubridate::year(bike$date)
bike[,13:17]<-lapply(bike[,13:17], as.factor)

season_summary <- ddply(bike,.(season,hour), summarise, count = mean(count))

########______________________ ##### 
#### PART2. 作圖 Y與X關係 ####
  ##### 01. 3維 顏色折線 ####

  # 四季小時 3維
ggplot(bike, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season), size=3.5) +
  geom_line(data = season_summary, aes(group = season), size=2) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_color_manual(values=c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"))

  # 周+小時 三維
summary <- ddply(bike,.(Weekday,hour), summarise, count = mean(count))

ggplot(bike, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = Weekday_summary, aes(group = Weekday), size=3.5) +
  geom_line(data = Weekday_summary, aes(group = Weekday), size=2) +
  scale_x_discrete("hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_color_brewer(palette="Set3")


######## 12月+小時 三維
month_summary <- ddply(bike,.(month,hour), summarise, count = mean(count))

ggplot(bike, aes(x = as.factor(hour), y = count, colour = as.numeric(month))) +
  geom_point(data = month_summary, aes(group = month), size=2.5, show.legend = F) +
  geom_line(data = month_summary, aes(group = month), size=1.5, show.legend = F) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count")+ 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_color_gradientn(colours = c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"))

# 假日+小時 3維
holiday_summary <- ddply(bike,.(holiday,hour), summarise, count = mean(count))

ggplot(bike, aes(x = as.factor(hour), y = count, color= holiday)) +
  geom_point(data = holiday_summary, aes(group= holiday), size=3, show.legend = F) +
  geom_line(data = holiday_summary, aes(group= holiday), size=2, show.legend =T) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count")+ 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_color_manual(values = c("#666666", "#CC9966"))

# 工作日+小時 3維
workingday_summary <- ddply(bike,.(workingday,hour), summarise, count = mean(count))

ggplot(bike, aes(x = as.factor(hour), y = count, color= workingday)) +
  geom_point(data = workingday_summary, aes(group= workingday), size=3, show.legend = F) +
  geom_line(data = workingday_summary, aes(group= workingday), size=2, show.legend =T) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count")+ 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_color_manual(values = c("#6E7783","#77AAAD"))

##### 02. 2維 BOX ######
# 12小時 BOX
  # pdf("atatime.pdf")  
ggplot(bike %>% group_by(hour) %>%
         mutate(medMPG = as.numeric(hour)),
       aes(x = hour, y= count)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_gradient2(midpoint = 12, low = "grey50", mid = "gold", high = "grey50")

  # scale_fill_manual(values=c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"))
  # dev.off()

# 季節 box 2維
ggplot(bike, aes(x = season, y = count, fill = season)) +
  geom_boxplot(aes(group = season), lwd=1.5) +
  scale_y_continuous("Count") +
  theme_minimal() +
  scale_fill_manual(values=c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"))+
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))

# 一周 box 2維
ggplot(bike %>% group_by(Weekday) %>%
         mutate(medMPG = as.numeric(Weekday))
       , aes(x = Weekday, y = count)) +
  geom_boxplot(aes(fill=Weekday), lwd=1.5, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("Month") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_brewer(palette="Set3")

# 12季 BOX
ggplot(bike %>% group_by(month) %>%
         mutate(medMPG = as.numeric(month))
       , aes(x = month, y = count)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("Month") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15))+
  scale_fill_gradientn(colours = c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"))

# 年 BOX
ggplot(bike, aes(x = year, y = count, fill = year)) +
  geom_boxplot(aes(group = year), lwd=1.5, show.legend = F) +
  scale_y_continuous("Count") +
  theme_minimal() +
  scale_fill_manual(values= c("#CC9999","#CCCC99"))+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
        axis.title.y = element_text(size = 25), axis.title.x = element_text(size = 25),
        axis.line=element_line(color="gray",size=1))

#  holiday
ggplot(bike %>% group_by(holiday) %>%
         mutate(medMPG = as.numeric(holiday))
       , aes(x = holiday, y = count)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("Holiday") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_gradientn(colours = c("#666666", "#CC9966"))

# workingday
ggplot(bike %>% group_by(workingday) %>%
         mutate(medMPG = as.numeric(workingday))
       , aes(x = workingday, y = count)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("workingday") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_gradientn(colours = c("#6E7783","#77AAAD"))

# weather
ggplot(bike, aes(x = weather, y = count, fill = weather)) +
  geom_boxplot(lwd=1.2, show.legend = T) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("weather") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_fill_manual(values = c("#4D96B9", "#D3D0C6", "#617078", "#30414B"))

# temp
ggplot(bike, aes(x = temp, y = count)) +
  geom_point(size=3, alpha= I(1/10),  color= "#B32D2D") +
  geom_line(data = bike%>% group_by(temp) %>% summarise(count= mean(count))
            , color= "#FA8888", size= 2) +
  scale_x_continuous("temp") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        panel.background = element_rect(fill = "snow",colour = "white",size = 0.5,linetype = "solid"),
        axis.line=element_line(color="gray",size=1))

##### ______________________ ####
##### PART3. GGpairs ####

ggpairs(data = bike[, c("season", "holiday", "workingday", "weather", "temp", "atemp", "humidity",
                        "windspeed", "count", "month", "Weekday")], 
        upper = list(continuous = "cor", discrete = "blank", combo = "blank"),
        lower = list(continuous = "points", discrete = "facetbar", combo = "box"),
        diag = list(continuous = "densityDiag", discrete = "barDiag"),
        axisLabels = "show", showStrips = T, title = "Scatter Plot & Correlation of Q1")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

# 條透明度
my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_point(..., alpha = I(1/10)) 
}
ggpairs(data = bike[, c("temp", "atemp", "humidity", "windspeed", "count")], 
        upper = list(continuous = "cor", discrete = "blank", combo = "blank"),
        lower = list(continuous = my_dens, discrete = "facetbar", combo = "box"),
        diag = list(continuous = "densityDiag", discrete = "barDiag"))+
  theme(plot.subtitle = element_text(size= 25))

##### ______________________ ####
##### PART4. Scatter ####
# humidity
ggplot(bike, aes(x = humidity, y = count)) +
  geom_point(size=3, alpha= I(2/10),  color= "#5A8FC3") +
  geom_line(data = bike%>% group_by(humidity) %>% summarise(count= mean(count))
            , color= "#3B72A8", size= 2) +
  scale_x_continuous("humidity") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        # panel.background = element_rect(fill = "azure",colour = "white",size = 0.5,linetype = "solid"),
        axis.line=element_line(color="gray",size=1))

# windspeed
ggplot(bike, aes(x = windspeed, y = count)) +
  geom_point(size=3, alpha= I(1/10),  color= "#86A538") +
  geom_line(data = bike%>% group_by(windspeed) %>% summarise(count= mean(count))
            , color= "#669D35", size= 2) +
  scale_x_continuous("windspeed") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        # panel.background = element_rect(fill = "azure",colour = "white",size = 0.5,linetype = "solid"),
        axis.line=element_line(color="gray",size=1))

# temp
ggplot(bike, aes(x = temp, y = count)) +
  geom_point(size=3, alpha= I(1/10),  color= "#B32D2D") +
  geom_line(data = bike%>% group_by(temp) %>% summarise(count= mean(count))
            , color= "#FA8888", size= 2) +
  scale_x_continuous("temp") +
  scale_y_continuous("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        panel.background = element_rect(fill = "snow",colour = "white",size = 0.5,linetype = "solid"),
        axis.line=element_line(color="gray",size=1))

##### ______________________ ####
##### PART5. Bar ####
# weather
ggplot(data = bike, aes(x= weather)) +
  geom_bar(fill= c("#4D96B9", "#D3D0C6", "#617078", "#30414B"), color="black", lwd= 1.5) +
  scale_y_continuous("numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


sum(bike$weather== "Clear or cloudy")
sum(bike$weather== "Mists")
sum(bike$weather== "Light rain or snow")
sum(bike$weather== "Heavy rain")

# season
ggplot(data = bike, aes(x= season)) +
  geom_bar(fill= c("#70B443", "#DE6059", "#D79A5A", "#5B8AC3"), color="black", lwd= 1.5) +
  scale_y_continuous("numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

sum(bike$season=="Spring")
sum(bike$season== "Summer")
sum(bike$season== "Fall")
sum(bike$season== "Winter")

# holiday
ggplot(data = bike, aes(x= holiday)) +
  geom_bar(fill= c("#666666", "#CC9966"), color="black", lwd= 1.5, width = .6) +
  scale_y_continuous("numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

sum(bike$holiday==0)
sum(bike$holiday==1)

# workingday
ggplot(data = bike, aes(x= workingday)) +
  geom_bar(fill= c("#6E7783", "#77AAAD"), color="black", lwd= 1.5, width = .6) +
  scale_y_continuous("numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

sum(bike$workingday==0)
sum(bike$workingday==1)

# bike %>% group_by(Weekday) %>% summarise(work = sum(as.numeric(as.character(bike$workingday))))

t0<- bike[bike$workingday == 0,]
t1<- bike[bike$workingday == 1,]
t.test(t0$count, t1$count, var.equal = T)
summary(aov(count~workingday, bike))
sum(tt$workingday == 0)

