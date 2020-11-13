# 時間區間切割
library(dplyr)
library(corrplot)
library(car)
library(MLmetrics)
library(boot)
library(caret)

hoursp <- bike
hoursp$hour <- as.numeric(as.character(hoursp$hour))
# group1 <- which(hoursp$hour== 0|hoursp$hour== 1| hoursp$hour== 2| hoursp$hour== 3)

# hoursp$hourc[hoursp$hour <24] <- "4"
# hoursp$hourc[hoursp$hour <18] <- "3"
# hoursp$hourc[hoursp$hour <12] <- "2"
# hoursp$hourc[hoursp$hour <6] <- "1"

hoursp$hourc[hoursp$hour <24] <- "6"
hoursp$hourc[hoursp$hour <20] <- "5"
hoursp$hourc[hoursp$hour <16] <- "4"
hoursp$hourc[hoursp$hour <12] <- "3"
hoursp$hourc[hoursp$hour <8] <- "2"
hoursp$hourc[hoursp$hour <4] <- "1"

hoursp$hourc[hoursp$hour <24] <- "8"
hoursp$hourc[hoursp$hour <21] <- "7"
hoursp$hourc[hoursp$hour <18] <- "6"
hoursp$hourc[hoursp$hour <15] <- "5"
hoursp$hourc[hoursp$hour <12] <- "4"
hoursp$hourc[hoursp$hour <9] <- "3"
hoursp$hourc[hoursp$hour <6] <- "2"
hoursp$hourc[hoursp$hour <3] <- "1"

hoursp$hour <- as.factor(hoursp$hour)
hoursp$hourc <- as.factor(hoursp$hourc)
###############################################################################################
###############################################################################################
# 去掉極端值
library(data.table)
library(dplyr)
library(plyr)

hist(model_lm_bike$count)
# 算出IQR
Q <- quantile(bike$count, c(0.25, 0.75))
IQR <- Q[2] - Q[1]

# 1.5IQR為標準
Q[2]+ 2.5*IQR

# 找出有幾個
sum((hoursp$count > (Q[2]+ 2*IQR)))

# 取出
outlier <- hoursp[hoursp$count > (Q[2]+ 2*IQR), ]
Nooutlier <- hoursp %>% dplyr::filter(hoursp$count < (Q[2]+ 2*IQR))


set<- sample(1:10583)
###############################################################################################
# 補值 
sd(bike$count)

hist(model_lm_bike$count)
# 算出IQR
Q <- quantile((bike$count- 191.6)/181.1445, c(0.25, 0.75))
IQR <- Q[2] - Q[1]

# 1.5IQR為標準
Q[2]+ 1.5*IQR

# 找出有幾個
sum(((hoursp$count- 191.6)/181.1445) > 3)
sum((hoursp$count < (Q[1]- 1.5*IQR)))
# 取出
outlier <- hoursp[((hoursp$count- 191.6)/181.1445) > 3, ]
Nooutlier <- hoursp %>% dplyr::filter(hoursp$count < (Q[2]+ 2*IQR))


set<- sample(1:10583)
###############################################################################################
###############################################################################################
# 殘差圖發現右偏，將count取log
# lm
Nooutlier_bike <- Nooutlier %>% mutate(count= log(count))
mean(Nooutlier_bike$count)
sd(Nooutlier_bike$count)

model_lm_n <- lm(scale(count)~ holiday*hour+ weather+ scale(temp)+ scale(humidity)+ month +year+ day*hour
                , data = Nooutlier_bike)

summary(model_lm_n)
vif(model_lm_n)

plot(model_lm_n)

##################
# Validation set #
##################
train <- sample(10886, 10886/2)

model_lm_train <- lm(count~ holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
                     , data = Nooutlier_bike, subset = train)

summary(model_lm_train)

# 手動算rmsle
y <- exp(Nooutlier_bike$count[-train])
p <- exp(predict(model_lm_train, Nooutlier_bike)[-train])


sq <- (log(y+1)- log(p+1))^2
sqrt(mean(sq))

# MLmetrics算rmsle
RMSLE(p, y)

# LOOCV
# 拉出Heavy那一個

model_glm_train <- glm(count~ holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
                       , data = Nooutlier_bike)
summary(model_glm_train)

kfold <- Nooutlier_bike[Nooutlier_bike$weather != "Heavy rain", ]

# K fold
cv <- cv.glm(kfold, model_glm_train, K = 10)
cv$delta

###############################################################################################
###############################################################################################
# poisson
model_poi_N<- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                         , data = Nooutlier, family = "poisson")
summary(model_poi)

1 - pchisq(model_poi$deviance, model_poi$df.residual)
1 - pchisq(model_poi$null.deviance-model_poi$deviance, df=1)

plot(model_poi)

##################
# Validation set #
##################
train <- sample(10886, 10886/2)

model_poi_train <- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                       , data = Nooutlier, family = "poisson", subset = train)

# 手動算rmsle
y <- Nooutlier$count[-train]
p <- exp(predict(model_poi_train, Nooutlier)[-train])

sq <- (log(y+1)- log(p+1))^2
sqrt(mean(sq))

# MLmetrics算rmsle
RMSLE(p, y)

kfold <- Nooutlier[Nooutlier$weather != "Heavy rain", ]
# K fold
cv <- cv.glm(kfold, model_poi_train, K = 10)
cv$delta

###############################################################################################
# 分開估計 
split_up
Nooutlier_bike <- Nooutlier %>% 
  mutate(casual= log(casual), registered= log(registered))

Nooutlier_bike$casual[Nooutlier_bike$casual== -Inf] <- 0
Nooutlier_bike$registered[Nooutlier_bike$registered== -Inf] <- 0

Nooutlier_bike$year <- as.numeric(Nooutlier_bike$year)

model_lm_casual <- lm(casual~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month +year
                      , data = Nooutlier_bike)

summary(model_lm_casual)

model_lm_registered <- lm(registered~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month +year
                          , data = Nooutlier_bike)
summary(model_lm_registered)

Nooutlier_bike$casual_P <- exp(predict(model_lm_casual, Nooutlier_bike))
Nooutlier_bike$registered_P <- exp(predict(model_lm_registered, Nooutlier_bike))

comparison <- data.frame(pre= Nooutlier_bike$casual_P+ Nooutlier_bike$registered_P,
                         ord= Nooutlier_bike$count )
###############################################################################################
library(caret)
library(lapply)
folds <- createFolds(Nooutlier$datetime)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = Nooutlier)

###############################################################################################
hist(bike$count)
hist(lm_bike$count)
hist(Nooutlier$count)

hist(Nooutlier_bike$count)
