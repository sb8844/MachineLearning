library(quantmod)
library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

getSymbols(c("GSPC","TSLA"), from = "2010-09-01",to="2017-11-12")
prices <- cbind(TSLA$TSLA.Adjusted,GSPC$GSPC.Adjusted)
returns <- diff(log(prices))
returns <- returns[-1]
acf()
returns$Lag1 <- lag(returns$TSLA.Adjusted)
returns$Lag2 <- lag(returns$TSLA.Adjusted,2)
returns$Lag3 <- lag(returns$TSLA.Adjusted,3)
returns <- na.omit(returns)
returns.train <- returns["2010-09-09/2016-01-27"]
returns.test <- returns["2016-01-28/2017-11-12"]

# trainindex <- createDataPartition(1:nrow(returns),p=.75,list=FALSE)
View(returns)

fit <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train,type= "eps-regression",gamma = .5)
fit1 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2,data = returns.train)
fit2 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1,data = returns.train)
fit3 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted ,data = returns.train)
fit4 <- svm(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train)
fit5 <- svm(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2,data = returns.train)

# ftable(predict(fit),returns$TSLA.Adjusted)
lmfit <- lm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train)


fcst <- predict(fit,newdata = returns.test)
fcst <- fcst[(length(returns.train[,1])+2):(length(fcst))]
fcst1 <- predict(fit1,newdata = returns.test)
fcst1 <- fcst1[(length(returns.train[,1])+2):(length(fcst1))]
fcst2 <- predict(fit2,newdata = returns.test)
fcst2 <- fcst2[(length(returns.train[,1])+2):(length(fcst2))]
fcst3 <- predict(fit3,newdata = returns.test)
fcst3 <- fcst3[(length(returns.train[,1])+2):(length(fcst3))]
fcst4 <- predict(fit4,newdata = returns.test)
fcst4 <- fcst4[(length(returns.train[,1])+2):(length(fcst4))]
fcst5 <- predict(fit5,newdata = returns.test)
fcst5 <- fcst5[(length(returns.train[,1])+2):(length(fcst5))]

dataframe <- data.frame(returns.test$TSLA.Adjusted,fcst,fcst1,fcst2,fcst3,fcst4,fcst5)


actualandfcst <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst))
names(actualandfcst) <- c("Actual","Forecast")
index(actualandfcst) <- as.Date(index(actualandfcst))
tidy(actualandfcst)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 0 and Actual Values")+ geom_line() + theme_minimal()

actualandfcst1 <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst1))
names(actualandfcst1) <- c("Actual","Forecast")
index(actualandfcst1) <- as.Date(index(actualandfcst1))
tidy(actualandfcst1)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 1 and Actual Values")+ geom_line() + theme_minimal()

actualandfcst2 <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst2))
names(actualandfcst2) <- c("Actual","Forecast")
index(actualandfcst2) <- as.Date(index(actualandfcst1))
tidy(actualandfcst2)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 2 and Actual Values")+ geom_line() + theme_minimal()


actualandfcst3 <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst3))
names(actualandfcst3) <- c("Actual","Forecast")
index(actualandfcst3) <- as.Date(index(actualandfcst3))
tidy(actualandfcst3)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 3 and Actual Values")+ geom_line() + theme_minimal()


actualandfcst4 <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst4))
names(actualandfcst4) <- c("Actual","Forecast")
index(actualandfcst4) <- as.Date(index(actualandfcst4))
tidy(actualandfcst4)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 4 and Actual Values")+ geom_line() + theme_minimal()


actualandfcst5 <- as.xts(data.frame(returns.test$TSLA.Adjusted,fcst5))
names(actualandfcst5) <- c("Actual","Forecast")
index(actualandfcst5) <- as.Date(index(actualandfcst5))
tidy(actualandfcst5)%>%ggplot(aes(x=index,y=value, color=series))+ labs(x= "Time",y="Returns") + ggtitle("Forecast 5 and Actual Values")+ geom_line() + theme_minimal()





plot(fcst,type="l",main="forecast")
plot(fcst1,type="l",main="forecast1")
plot(fcst2,type="l",main="forecast2")
plot(fcst3,type="l",main="forecast3")
plot(fcst4,type="l",main="forecast4")
plot(fcst5,type="l",main="forecast5")

#fix lengths
fe <- returns.test[,1] - fcst
fe1 <- returns.test[,1] - fcst1
fe2 <- returns.test[,1] - fcst2
fe3 <- returns.test[,1] - fcst3
fe4 <- returns.test[,1] - fcst4
fe5 <- returns.test[,1] - fcst5

#square the errors
feErrorSquared <- fe^2
feErrorSquared1 <- fe1^2
feErrorSquared2 <- fe2^2
feErrorSquared3 <- fe3^2
feErrorSquared4 <- fe4^2
feErrorSquared5 <- fe5^2

#Mean Squared Errors
mse <- mean(feErrorSquared)
mse1 <- mean(feErrorSquared1)
mse2 <- mean(feErrorSquared2)
mse3 <- mean(feErrorSquared3)
mse4 <- mean(feErrorSquared4)
mse5 <- mean(feErrorSquared5)

rmse <- sqrt(mean(feErrorSquared))
rmse1 <- sqrt(mean(feErrorSquared1))
rmse2 <- sqrt(mean(feErrorSquared2))
rmse3 <- sqrt(mean(feErrorSquared3))
rmse4 <- sqrt(mean(feErrorSquared4))
rmse5 <- sqrt(mean(feErrorSquared5))

plot(c(mse,mse1,mse2,mse3,mse4,mse5))
#forecast error and accuracies
plot(fe,type="l",,main="Forecast Error")
plot(feErrorSquared,type="l",,main="Forecast Accuracy")
plot(fe1,type="l",main="Forecast1 error")
plot(feErrorSquared1,type="l",main="Forecast1 Accuracy")
plot(fe2,type="l",main="Forecast2 Error")
plot(feErrorSquared2,type="l",main="Forecast2 Accuracy")
plot(fe3,type="l",main="Forecast3 Error")
plot(feErrorSquared3,type="l",main="Forecast3 Accuracy")
plot(fe4,type="l",main="Forecast4 Error")
plot(feErrorSquared4,type="l",main="Forecast4 Accuracy")
plot(fe5,type="l",main="Forecast5 Error")
plot(feErrorSquared5,type="l",main="Forecast5 Accuracy")
# scatter3D(fit$fitted,returns$GSPC.Adjusted,pch=18,cex=2,theta = 20,phi=20,surf = list(x=fit$fitted,y=returns$GSPC.Adjusted))
