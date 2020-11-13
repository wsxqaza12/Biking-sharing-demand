
## 1.Y (count)

# 1.1 histogram
pdf("C:/Users/User/Desktop/研究所/碩一下/廣義線性模式應用分析/bike sharing demand/histogramall.pdf")
ggplot(data = bike, aes(x= count)) +
  geom_histogram(aes(y=..density..), fill= "#8FBC94", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  scale_x_continuous(limits = c(-100, 1000))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))
dev.off()

# 1.2 boxplot
pdf("C:/Users/User/Desktop/研究所/碩一下/廣義線性模式應用分析/bike sharing demand/boxall1.pdf")
ggplot(data = bike) +
  geom_boxplot(aes(y= count), fill= "#CC9966", lwd=1)+
  scale_x_discrete(breaks=NULL)+
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 20), 
        axis.title.y = element_text(size = 30))
dev.off()

# 1.2 boxplot 倒
ggplot(data = bike) +
  geom_boxplot(aes(y= count), fill= "#8FBC94", lwd=1.5, outlier.color = "#548687")+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(limits = c(-100, 1000))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 25), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 30),
        axis.line=element_line(color="gray",size=1),
        axis.line.y = element_line(color="gray",size=1))+
  coord_flip()
##################################################################################
## 2.time 年
# 2.1 年

#> 2.1.1 histogram

yearman11 <- bike[bike$year==2011, ] %>%
  group_by(month) %>% summarise(count = sum(count))
  
yearman12 <- bike[bike$year==2012, ] %>%
  group_by(month) %>% summarise(count = sum(count))

yearman <- rbind(yearman11, yearman12)
yearman[, 3] <- c(rep(2012, 12), rep(2013, 12))

ggplot(data = bike) +
  geom_boxplot(aes(y= count), fill= "#CC9966", lwd=1.2)

# 年趨勢
bike$yemon <- substr(bike$datetime, 1, 7)
yeartrend <- bike %>% group_by(yemon) %>% summarise(count= mean(count))
yeartrend[, 1] <- 1:24

ggplot(data = bike, aes(x= yemon, y= count), color= year) +
  geom_jitter(aes(color=year), width=.3, alpha = I(5/10), show.legend = F) +
  geom_line(data= yeartrend, aes(x= yemon, y= count), lwd= 1.5)+
  scale_x_discrete("Time", labels= rep(1:12, 2)) +
  scale_y_continuous("Count") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 25), axis.title.x = element_text(size = 25),
        axis.line=element_line(color="gray",size=1),
        axis.line.y = element_line(color="gray",size=1))+
  geom_vline(xintercept = 12.5, linetype="dashed", colour="gray20")+
  scale_color_manual(values= c("#CC9999","#CCCC99"))



# ######################
library(dplyr)
mtcars
ggplot(mtcars %>% group_by(carb) %>%
         mutate(medMPG = median(mpg)), 
       aes(x = reorder(carb, mpg, FUN=median), y = mpg)) +
  geom_boxplot(aes(fill=medMPG)) +
  stat_summary(fun.y=mean, colour="darkred", geom="point") +
  scale_fill_gradient(low=hcl(15,100,75), high=hcl(195,100,75))

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

##################################################################################
# 算時間的table
# hour
hour <- bike %>% group_by(hour) %>% summarise(mean= mean(count), sd= sd(count),
                                             min = min(count), "25%" = quantile(count, 0.25),
                                             "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                             max= max(count))

# day
day <- bike %>% group_by(day) %>% summarise(mean= mean(count), sd= sd(count),
                                              min = min(count), "25%" = quantile(count, 0.25),
                                              "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                              max= max(count))
summary(aov(count~day, bike))

# month
month <- bike %>% group_by(month) %>% summarise(mean= mean(count), sd= sd(count),
                                             min = min(count), "25%" = quantile(count, 0.25),
                                             "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                             max= max(count))
summary(aov(count~month, bike))

# year
year <- bike %>% group_by(year) %>% summarise(mean= mean(count), sd= sd(count),
                                                min = min(count), "25%" = quantile(count, 0.25),
                                                "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                                max= max(count))
summary(aov(count~year, bike))

# season
season <- bike %>% group_by(season) %>% summarise(mean= mean(count), sd= sd(count),
                                                    min = min(count), "25%" = quantile(count, 0.25),
                                                    "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                                    max= max(count))
summary(aov(count~season, bike))

# holiday
holiday <- bike %>% group_by(holiday) %>% summarise(mean= mean(count), sd= sd(count),
                                                    min = min(count), "25%" = quantile(count, 0.25),
                                                    "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                                    max= max(count))
summary(aov(count~holiday, bike))

# workingday
workingday <- bike %>% group_by(workingday) %>% summarise(mean= mean(count), sd= sd(count),
                                                    min = min(count), "25%" = quantile(count, 0.25),
                                                    "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                                    max= max(count))
summary(aov(count~workingday, bike))

# weather
weather <- bike %>% group_by(weather) %>% summarise(mean= mean(count), sd= sd(count),
                                                min = min(count), "25%" = quantile(count, 0.25),
                                                "50%" = quantile(count, 0.5), "75%" = quantile(count, 0.75),
                                                max= max(count))
summary(aov(count~weather, bike))

# temp
cor(bike$temp, bike$count)

# humidity
cor(bike$humidity, bike$count)

# windspeed
cor(bike$windspeed, bike$count)


