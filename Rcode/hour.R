# Box-Cox
library(MASS)
library(car)
library(randomForest)
library(glmnet)
library(dplyr)
library(ggplot2)
library(caret)
library(MLmetrics)
library(boot)
# find new x
hoursp <- bike
hoursp$hour <- as.numeric(as.character(hoursp$hour))

# try 
hoursp$hour_lasso[hoursp$hour ==6] <- "1"
hoursp$hour_lasso[hoursp$hour ==7] <- "2"
hoursp$hour_lasso[hoursp$hour >9 & hoursp$hour<18] <- "3"
hoursp$hour_lasso[hoursp$hour ==8] <- "4"
hoursp$hour_lasso[hoursp$hour ==9] <- "5"
hoursp$hour_lasso[hoursp$hour ==20| hoursp$hour==21] <- "6"
hoursp$hour_lasso[hoursp$hour ==19| hoursp$hour==18] <- "7"
hoursp$hour_lasso[hoursp$hour ==22| hoursp$hour==23] <- "8"
hoursp$hour_lasso[hoursp$hour <6] <- "9"
hoursp$hour_lasso[hoursp$hour ==17] <- "10"

# hour cate for registered users
hoursp$hour_reg[hoursp$hour <8] <- "1"
hoursp$hour_reg[hoursp$hour >=22] <- "2"
hoursp$hour_reg[hoursp$hour >9 & hoursp$hour<18] <- "3"
hoursp$hour_reg[hoursp$hour ==8] <- "4"
hoursp$hour_reg[hoursp$hour ==9] <- "5"
hoursp$hour_reg[hoursp$hour ==20| hoursp$hour==21] <- "6"
hoursp$hour_reg[hoursp$hour ==19| hoursp$hour==18] <- "7"

# hour cate for casual users
hoursp$hour_cas[hoursp$hour<=8] <- "1"
hoursp$hour_cas[hoursp$hour == 9] <- "2"
hoursp$hour_cas[hoursp$hour >=10 & hoursp$hour<=19] <- "3"
hoursp$hour_cas[hoursp$hour >19] <- "4"

## Day Type: Created a variable having categories like “weekday”, “weekend” and “holiday”. 
hoursp$day_type[hoursp$holiday==0 & hoursp$workingday==0] <- "weekend"
hoursp$day_type[hoursp$holiday==1] <- "holiday"
hoursp$day_type[hoursp$holiday==0 & hoursp$workingday==1] <- "working day"

## Weekend:Created a separate variable for weekend (0/1) 
hoursp$weekend <- 0
hoursp$weekend[hoursp$day=="7" | hoursp$day=="1"] <- 1

hoursp$hour <- as.factor(hoursp$hour)
hoursp$hour_lasso <- as.factor(hoursp$hour_lasso)


hoursp_registered <- hoursp %>% 
  filter(registered> 0) %>%
  mutate(registered= log(registered)) 

  cbind(hoursp_registered, hoursp[hoursp$registered==0, ])

##############################################################
Nooutlier_5632 <- Nooutlier[-c(5632), ]
Nooutlier_5632 <- hoursp[-c(5632), ]
folds <- createFolds(Nooutlier_5632$datetime)

test_fold <- lapply(folds, function(ind, dat) dat[ind,], dat = Nooutlier_5632)
train_fold <- lapply(folds, function(ind, dat) dat[-ind,], dat = Nooutlier_5632)

# model for test
model <- glm(log(count)~ holiday*hour_lasso+ weather+  humidity+ month +year+ day +atemp
             , data = hoursp)
vif(model)
alias(model)
names(hoursp)

##############################################################
#Gaussian & Box Cox
Rmsle <- rep(0, 11)
MSE <- rep(0, 11)
for(i in 1:10){
  model <- lm(count^0.1818182~ day_type+ weather+  humidity+ month +year+  atemp*hour_reg+ day*hour_reg
              , data = train_fold[[i]] %>% cbind(., Nooutlier[5632,]))
  
  y <- test_fold[[i]]$count
  p <- predict(model, test_fold[[i]])^(1/0.1818182)
  
  
  Rmsle[i] <- RMSLE(p, y)
  Rmsle[i+1] <- mean(Rmsle)
  MSE[i] <- Ms(p, y)
  MSE[i+1] <- mean(MSE)
}

print(c(Rmsle, MSE))

##############################################################
#Gaussian & log
Rmsle <- rep(0, 11)
for(i in 1:10){
  model <- lm(log(count)~ holiday*hour_lasso+ weather+  humidity+ month +year+ windspeed+day*hour_lasso
              , data = train_fold[[i]] %>% cbind(., hoursp[5632,]))
  
  y <- test_fold[[i]]$count
  p <- exp(predict(model, test_fold[[i]]))
  
  
  Rmsle[i] <- RMSLE(p, y)
  Rmsle[i+1] <- mean(Rmsle[1:10])

  MSE[i] <- sqrt(mean((y-p)^2))
  MSE[i+1] <- mean(MSE)
}

print(MSE)
print(Rmsle)
plot(model)
sink("C:/Users/User/Desktop/研究所/碩一下/廣義線性模式應用分析/bike sharing demand/beta.txt")
summary(model)
sink()
##############################################################
# Box-Cox
b <- boxcox(count~holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour
            ,data=Nooutlier)

I <- which(b$y== max(b$y))
b$x[I]

c <- lm(count^0.1818182~holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour
        ,data= lm_bike)

plot(c)

##############################################################
# K mean to hours
hourk <- hoursp[, c(12, 13)]

hourkmean <- kmeans(hourk, centers = 12)

plot(hourk$count, hourk$hour, data = hourk, col = hourkmean$cluster,
     main = "將鳶尾花做分群", xlab = "花瓣寬度", ylab = "花瓣長度")

model <- rlm(log(count)~ holiday*hour+ weather+  humidity+ month +year+ day*hour+atemp+ windspeed
             , data = hoursp)

summary(model)

##############################################################
# randomForest
fit1 <- randomForest(log(count) ~ holiday*hour+ weather+  humidity+ month +year+ day*hour+atemp*hour+ ,
                     data=train_fold[[1]], importance=TRUE, ntree=250)

y <- test_fold[[1]]$count
p <- exp(predict(fit1, test_fold[[1]]))

RMSLE(p, y)
##############################################################
# Ridge regression

x <- model.matrix(log(count)~ holiday*hour+ weather+  humidity+ month +year+ day*hour+atemp*hour
                  , data = hoursp)[, -1]

y <- log(hoursp$count)
# y <- hoursp %>% mutate(logcount = log(count)) %>% .[, 22]

dim(x)

# fit
ames_ridge <- glmnet(
  x = x,
  y = y,
  alpha = 1
)

plot(ames_ridge, xvar = "lambda")

# CV
ames_ridge <- cv.glmnet(
  x = x,
  y = y,
  alpha = 1
)

plot(ames_ridge)
min(ames_ridge$cvm)

ames_ridge_min <- glmnet(
  x = x,
  y = y,
  alpha = 1
)

{
  plot(ames_ridge_min, xvar = "lambda")
  abline(v = log(ames_ridge$lambda.1se), col = "red", lty = "dashed")
}

coef(ames_ridge, s = "lambda.1se") %>% # 308 x 1 sparse Matrix of class "dgCMatrix"
  as.matrix() %>% 
  as.data.frame() %>% 
  add_rownames(var = "var") %>% 
  `colnames<-`(c("var","coef")) %>%
  filter(var != "(Intercept)") %>%  #剔除截距項
  top_n(25, wt = coef) %>% 
  ggplot(aes(coef, reorder(var, coef))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


##############################################################
Nooutlier_5632 <- Nooutlier[-c(5632), ]
Nooutlier_5632 <- hoursp[-c(5632), ]
folds <- createFolds(Nooutlier_5632$datetime)

test_fold <- lapply(folds, function(ind, dat) dat[ind,], dat = Nooutlier_5632)
train_fold <- lapply(folds, function(ind, dat) dat[-ind,], dat = Nooutlier_5632)


for(i in 1:10){
x <- model.matrix(log(count)~ holiday*hour+ weather+  humidity+ month +year+ day*hour+atemp*hour+ weekend+
                    day_type+ hour_lasso
                  , data = train_fold[[i]])[, -1]

y <- log(train_fold[[i]]$count)

ames_ridge_min <- cv.glmnet(
  x = x,
  y = y,
  alpha = 1
)

x_test <- model.matrix(log(count)~ holiday*hour+ weather+  humidity+ month +year+ day*hour+atemp*hour+ weekend+
                         day_type+ hour_lasso+ holiday*hour_lasso+ workingday
                       , data = test_fold[[i]])[, -1]
y_test <- test_fold[[i]]$count
  
p <- predict(ames_ridge_min, newx = x_test, s = ames_ridge_min$lambda.min)

Rmsle[i] <- RMSLE(exp(p), y_test)
Rmsle[i+1] <- mean(Rmsle[1:10])


}


print(Rmsle)
R_square  
coef(ames_ridge_min, s = "lambda.min")

# install.packages('MplusAutomation')
library(MplusAutomation)
library(MASS);library(Matrix);library(reshape2)
library(magrittr);library(ggplot2)
setwd("C:/Users/User/Desktop")
runModels("Davies.inp")
M = readModels()



