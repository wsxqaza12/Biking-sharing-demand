ggplot(data = bike, aes(x= count^0.1818182)) +
  geom_histogram(aes(y=..density..), fill= "#8FBC94", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  scale_x_continuous(limits = c(0, 4))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))
dev.off()


ggplot(data = bike, aes(x= log(count))) +
  geom_histogram(aes(y=..density..), fill= "#8FBC94", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  scale_x_continuous(limits = c(-1, 9))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))
dev.off()

shapiro.test(bike$count^0.1818182)
ks.test(bike$count^0.1818182, "pnorm", alternative = "two.sided")
ks.test(log(bike$count), "pnorm", alternative = "two.sided")


# BOXCOX
b <- boxcox(count~holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour
            ,data=Nooutlier)

I <- which(b$y== max(b$y))
b$x[I]

c <- lm(count^0.1818182~holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+windspeed
        ,data= lm_bike)
######################################
Rmsle <- rep(0, 11)
MSE <- rep(0, 11)
for(i in 1:10){
  model <- glm(count^0.1818182~ holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
              , data = train_fold[[i]] %>% cbind(., Nooutlier[5632,]))
  
  y <- test_fold[[i]]$count
  p <- predict(model, test_fold[[i]])^(1/0.1818182)
  Rmsle[i] <- RMSLE(p, y)
}
AIC(model)
Rmsle[11] <- mean(Rmsle[1:10])
print(Rmsle)     
summary(model)

######################################
model_lm_train <- lm(log(count)~ holiday*hour+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
                     , data = Nooutlier, subset = train)

##############################################################
Nooutlier_5632 <- Nooutlier[-c(5632), ]
Nooutlier_5632 <- hoursp[-c(5632), ]

folds <- createFolds(Nooutlier_5632$datetime)

test_fold <- lapply(folds, function(ind, dat) dat[ind,], dat = Nooutlier_5632)
train_fold <- lapply(folds, function(ind, dat) dat[-ind,], dat = Nooutlier_5632)

##############################################################
######################################
#Gaussian & log
Rmsle <- rep(0, 11)
for(i in 1:10){
  model <- lm(log(count)~ workingday+ weather+ temp+ humidity+ month +year+ day*hour
              , data = train_fold[[i]] %>% cbind(., Nooutlier[5632,]))
  
  y <- test_fold[[i]]$count
  p <- exp(predict(model, test_fold[[i]]))
  
  
  Rmsle[i] <- RMSLE(p, y)
}
Rmsle[11] <- mean(Rmsle[1:10])
print(Rmsle)
vif(model)

print(MSE)
print(Rmsle)
plot(model)
sink("C:/Users/User/Desktop/╯┮/河/約竡絬┦家Α莱ノだ猂/bike sharing demand/beta1.txt")
summary(model)
sink()

######################################  ######################################
# BOXCOX
Rmsle <- rep(0, 11)
for(i in 1:10){
  model <- lm(count^0.1818182~ holiday+ weather+ temp+ humidity+ month +year+ day*hour
              , data = train_fold[[i]] %>% cbind(., Nooutlier[5632,]))
  
  y <- test_fold[[i]]$count
  p <- predict(model, test_fold[[i]])^(1/0.1818182)
  
  
  Rmsle[i] <- RMSLE(p, y)
  Rmsle[i+1] <- mean(Rmsle)
  MSE[i] <- Ms(p, y)
  MSE[i+1] <- mean(MSE)
}

print(c(Rmsle, MSE))
      
plot(model)
######################################  ######################################
Nooutlier <- hoursp %>% dplyr::filter(hoursp$count < (Q[2]+ 2*IQR))
Nooutlier_5632 <- Nooutlier[-c(5632), ]
Nooutlier_5632 <- hoursp[-c(5632), ]
Nooutlier$hour <- relevel(Nooutlier$hour, 4)
Nooutlier$day <- relevel(Nooutlier$day, 3)
Nooutlier$weather <- relevel(Nooutlier$weather, )

contrasts(Nooutlier$hour)
contrasts(Nooutlier$weather)

model <- lm(log(count)~ holiday+ weather+ temp+ humidity+ month +year+ day*hour+ windspeed
            , data = Nooutlier)

sink("C:/Users/User/Desktop/╯┮/河/約竡絬┦家Α莱ノだ猂/bike sharing demand/beta5.txt")
summary(model)
sink()

model <- glm.nb(count~ holiday+ weather+ temp+ humidity+ month +year+ day*hour
                , data =Nooutlier)
summary(model)
