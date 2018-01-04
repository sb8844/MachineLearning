library(quantmod)
library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library(nnet)
library(neuralnet)
library(ggplot2)
library(broom)
library(magrittr)

getSymbols(c("GSPC","TSLA"), from = "2010-09-01",to="2017-11-12")
prices <- cbind(TSLA$TSLA.Adjusted,GSPC$GSPC.Adjusted)
returns <- diff(log(prices))
returns <- returns[-1]
returns <- as.data.frame(returns)
returns$Lag1 <- lag(returns$TSLA.Adjusted)
returns$Lag2 <- lag(returns$TSLA.Adjusted,2)
returns$Lag3 <- lag(returns$TSLA.Adjusted,3)
returns <- na.omit(returns)
returns.train <- as.data.frame(returns["2010-09-09/2016-01-27"])
returns.test <- returns["2016-01-28/2017-11-12"]

# trainindex <- createDataPartition(1:nrow(returns),p=.75,list=FALSE)
View(returns)

# fit <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train)
# fit1 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2,data = returns.train)
# fit2 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1,data = returns.train)
# fit3 <- svm(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted ,data = returns.train)
# fit4 <- svm(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train)
# fit5 <- svm(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2,data = returns.train)

fit.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train,linear.output = T)
fit1.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2,data = returns.train,linear.output = T)
fit2.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1,data = returns.train,linear.output = T)
fit3.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted ,data = returns.train,linear.output = T)
fit4.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train,linear.output = T)
fit5.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2,data = returns.train,linear.output = T)



# ftable(predict(fit),returns$TSLA.Adjusted)
# fcst <- predict(fit,newdata = returns.test)
# fcst <- fcst[(length(returns.train[,1])+1):(length(fcst))]
# fcst1 <- predict(fit1,newdata = returns.test)
# fcst1 <- fcst1[(length(returns.train[,1])+1):(length(fcst1))]
# fcst2 <- predict(fit2,newdata = returns.test)
# fcst2 <- fcst2[(length(returns.train[,1])+1):(length(fcst2))]
# fcst3 <- predict(fit3,newdata = returns.test)
# fcst3 <- fcst3[(length(returns.train[,1])+1):(length(fcst3))]
# fcst4 <- predict(fit4,newdata = returns.test)
# fcst4 <- fcst4[(length(returns.train[,1])+1):(length(fcst4))]
# fcst5 <- predict(fit5,newdata = returns.test)
# fcst5 <- fcst5[(length(returns.train[,1])+1):(length(fcst5))]


# ftable(predict(fit),returns$TSLA.Adjusted)
fcst.nn <- predict(fit,newdata = returns.test)
fcst.nn <- fcst[(length(returns.train[,1])+1):(length(fcst.nn))]
fcst1.nn <- predict(fit1.nn,newdata = returns.test)
fcst1.nn <- fcst1.nn[(length(returns.train[,1])+1):(length(fcst1.nn))]
fcst2.nn <- predict(fit2.nn,newdata = returns.test)
fcst2.nn <- fcst2.nn[(length(returns.train[,1])+1):(length(fcst2.nn))]
fcst3.nn <- predict(fit3.nn,newdata = returns.test)
fcst3.nn <- fcst3.nn[(length(returns.train[,1])+1):(length(fcst3.nn))]
fcst4.nn <- predict(fit4.nn,newdata = returns.test)
fcst4.nn <- fcst4.nn[(length(returns.train[,1])+1):(length(fcst4.nn))]
fcst5.nn <- predict(fit5.nn,newdata = returns.test)
fcst5.nn <- fcst5.nn[(length(returns.train[,1])+1):(length(fcst5.nn))]


index(fcst) <- index(returns.test$TSLA.Adjusted)
combined <- xts(merge(returns.test$TSLA.Adjusted,fcst))

plot(fcst,type="l",,main="forecast")
plot(fcst1,type="l",main="forecast1")
plot(fcst2,type="l",main="forecast2")
plot(fcst3,type="l",main="forecast3")
plot(fcst4,type="l",main="forecast4")
plot(fcst5,type="l",main="forecast5")

fe <- returns.test[,1] - fcst
fe1 <- returns.test[,1] - fcst1
fe2 <- returns.test[,1] - fcst2
fe3 <- returns.test[,1] - fcst3
fe4 <- returns.test[,1] - fcst4
fe5 <- returns.test[,1] - fcst5


plot(fe,type="l",,main="forecast er")
plot(fe1,type="l",main="forecast1 er")
plot(fe2,type="l",main="forecast2 er")
plot(fe3,type="l",main="forecast3 er")
plot(fe4,type="l",main="forecast4 er")
plot(fe5,type="l",main="forecast5 er")
# scatter3D(fit$fitted,returns$GSPC.Adjusted,pch=18,cex=2,theta = 20,phi=20,surf = list(x=fit$fitted,y=returns$GSPC.Adjusted))
