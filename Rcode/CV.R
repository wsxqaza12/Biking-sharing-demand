library(caret)
library(MLmetrics)
library(boot)
options('max.print' = 100000)

Nooutlier_5632 <- Nooutlier[-c(5632), ]
# lm_bike = bike, 無去極值的資料
Nooutlier_5632 <- lm_bike[-c(5632), ]
folds <- createFolds(Nooutlier_5632$datetime)

test_fold <- lapply(folds, function(ind, dat) dat[ind,], dat = Nooutlier_5632)
train_fold <- lapply(folds, function(ind, dat) dat[-ind,], dat = Nooutlier_5632)

##############################################################
#Gaussian
Rmsle <- rep(0, 11)
for(i in 1:10){
  model <- lm(log(count)~ holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
                       , data = train_fold[[i]] %>% cbind(., Nooutlier[5632,]))
  
  y <- test_fold[[i]]$count
  p <- exp(predict(model, test_fold[[i]]))
  

  Rmsle[i] <- RMSLE(p, y)
}
Rmsle[11] <- mean(Rmsle[1:10])
print(Rmsle)
vif(model)
##############################################################
# poisson
for(i in 1:10){
  model <- glm(count~ holiday+ weather+ temp+ humidity+ month +year+ day*hour
               , data = train_fold[[i]] %>% cbind(., Nooutlier_5632[5632,]), family = poisson())
  
  y <- test_fold[[i]]$count
  p <- exp(predict(model, test_fold[[i]]))
  
  
  Rmsle[i] <- RMSLE(p, y)
}
Rmsle[11] <- mean(Rmsle[1:10])
print(Rmsle)
summary(model)
##############################################################
##############################################################
# quasipoisson
for(i in 1:10){
    model <- glm(count~ holiday*hour+ weather+ temp+ humidity+ hour+ month+ year+ day*hour
                            , data = train_fold[[i]] %>% cbind(., Nooutlier_5632[5632,]), family = quasipoisson())
    
    y <- test_fold[[i]]$count
    p <- exp(predict(model, test_fold[[i]]))
    
    
    Rmsle[i] <- RMSLE(p, y)
}
Rmsle[11] <- mean(Rmsle[1:10])
print(Rmsle)
##############################################################
# NB
for(i in 1:10){
  model <- glm.nb(count~ holiday*hour+ weather+ temp+ humidity+ hour+ month+ year+ day*hour
               , data = train_fold[[i]] %>% cbind(., Nooutlier_5632[5632,]))
  
  y <- test_fold[[i]]$count
  p <- exp(predict(model, test_fold[[i]]))
  
  
  Rmsle[i] <- RMSLE(p, y)
  Rmsle[i+1] <- mean(Rmsle, na.rm = T)
}

print(Rmsle)
summary(model)
