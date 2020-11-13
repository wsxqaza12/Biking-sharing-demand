# linear regression
library(dplyr)
library(corrplot)
library(car)
library(MLmetrics)
library(boot)

# bike$times   <- strftime(bike$datetime, format="%H:%M:%S")
bike$hour <- hour(bike$datetime)
# bike$month <- lubridate::month(bike$date, label=TRUE)
bike$month <- lubridate::month(bike$date)
bike$day <- wday(bike$datetime)
bike$Weekday <- lubridate::wday(bike$date, label=TRUE)
bike$year <- lubridate::year(bike$date)
bike[,13:17]<-lapply(bike[,13:17], as.factor)

head(bike)
model_bike <- bike %>% dplyr::select(-datetime)

###############################################################################################
# poisson
names(model_bike)
model_poi<- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                , data = model_bike, family = "poisson")
summary(model_poi)

1 - pchisq(model_poi$deviance, model_poi$df.residual)
1 - pchisq(model_poi$null.deviance-model_poi$deviance, df=1)

plot(model_poi)

##################
# Validation set #
##################
train <- sample(10886, 10886/2)

model_poi_train <- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                      , data = model_bike, family = "poisson", subset = train)

# 手動算rmsle
y <- model_bike$count[-train]
p <- exp(predict(model_poi_train, model_bike)[-train])

sq <- (log(y+1)- log(p+1))^2
sqrt(mean(sq))

# MLmetrics算rmsle
RMSLE(p, y)

kfold <- model_bike[model_bike$weather != "Heavy rain", ]
# K fold
cv <- cv.glm(kfold, model_poi, K = 10)
cv$delta

cv.err.10 <- rep(0, 10)
for (i in 1:10) {
  cv.err.10[i] <- cv.glm(kfold, model_poi, K = 10)$delta[1]
}


cv.err.10
###############################################################################################
# quasipoisson

qpoi_bike <- bike %>% select(-datetime )
model_qpoi<- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                , data = qpoi_bike, family = "quasipoisson")
summary(model_qpoi)

1 - pchisq(model_poi$deviance, model_poi$df.residual)
1 - pchisq(model_poi$null.deviance-model_poi$deviance, df=1)

plot(model_qpoi)


train <- sample(10886, 10886/2)

model_qpoi_train <- glm(count~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                       , data = qpoi_bike, family = "quasipoisson", subset = train)

# 手動算rmsle
y <- qpoi_bike$count[-train]
p <- exp(predict(model_qpoi_train, qpoi_bike)[-train])

sq <- (log(y+1)- log(p+1))^2
sqrt(mean(sq))

# MLmetrics算rmsle
RMSLE(p, y)

kfold <- qpoi_bike[qpoi_bike$weather != "Heavy rain", ]
# K fold
cv <- cv.glm(kfold, model_qpoi, K = 10)
cv$delta

cv.err.10 <- rep(0, 10)
for (i in 1:10) {
  cv.err.10[i] <- cv.glm(kfold, model_qpoi, K = 10)$delta[2]
}
cv.err.10
###############################################################################################

# 殘差圖發現右偏，將count取log
# lm
lm_bike <- bike 

model_lm<- lm(log(count)~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month +year
                , data = lm_bike)

summary(model_lm)
plot(model_lm)

##################
# Validation set #
##################
train <- sample(10886, 10886/2)

model_lm_train <- lm(log(count)~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month+ year
                     , data = lm_bike, subset = train)
summary(model_lm_train)

model_lm_train <- lm(log(count)~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ season+ year+day*hour
                     , data = lm_bike, subset = train)
summary(model_lm_train)

model_lm_train <- lm(log(count)~ holiday*hour+ weather+ temp+ humidity+ hour+ season+ year+day*hour
                     , data = lm_bike, subset = train)
summary(model_lm_train)

# 
vif(model_lm_train)
# 手動算rmsle
y <- lm_bike$count[-train]
p <- exp(predict(model_lm_train, lm_bike)[-train])


sq <- (log(y+1)- log(p+1))^2
sqrt(mean(sq))

# MLmetrics算rmsle
RMSLE(p, y)

# LOOCV
# 拉出Heavy那一個
model_glm<- glm(log(count)~ holiday*hour+ workingday*hour+ weather+ temp+ humidity+ hour+ month +year+ day*hour
              , data = lm_bike)
summary(model_glm)

kfold <- lm_bike[lm_bike$weather != "Heavy rain", ]

# K fold
cv <- cv.glm(kfold, model_glm, K = 10)
cv$delta

# cv.err.10 <- rep(0, 10)
# for (i in 1:10) {
#   cv.err.10[i] <- cv.glm(kfold, model_glm, K = 10)$delta[1]
# }
# cv.err.10

###############################################################################################
