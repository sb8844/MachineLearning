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
returns.train <- returns["2010-09-09/2016-01-27"]
returns.test <- returns["2016-01-28/2017-11-12"]
returns.test1 <- cbind(returns.test[,2],returns.test[,3],returns.test[,4],returns.test[,4])


fit.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train,linear.output = T)
fit1.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1 +returns$Lag2,data = returns.train,linear.output = T)
fit2.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted + returns$Lag1,data = returns.train,linear.output = T)
fit3.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$GSPC.Adjusted ,data = returns.train,linear.output = T)
fit4.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2 + returns$Lag3,data = returns.train,linear.output = T)
fit5.nn <- neuralnet(returns$TSLA.Adjusted ~ returns$Lag1 +returns$Lag2,data = returns.train,linear.output = T)


# ftable(predict(fit),returns$TSLA.Adjusted)
fcst.nn <- compute(fit.nn,covariate = returns.test1)
fcst1.nn <- compute(fit1.nn,covariate = returns.test1)
fcst2.nn <- compute(fit2.nn,covariate = returns.test1)
fcst3.nn <- compute(fit3.nn,covariate = returns.test1)
fcst4.nn <- compute(fit4.nn,covariate = returns.test1)
fcst5.nn <- compute(fit5.nn,covariate = returns.test1)


fcst.nn <- fcst.nn[(length(returns.train1[,1])+1):(length(fcst.nn))]
fcst1.nn <- fcst1.nn[(length(returns.train1[,1])+1):(length(fcst1.nn))]
fcst2.nn <- fcst2.nn[(length(returns.train1[,1])+1):(length(fcst2.nn))]
fcst3.nn <- fcst3.nn[(length(returns.train1[,1])+1):(length(fcst3.nn))]
fcst4.nn <- fcst4.nn[(length(returns.train1[,1])+1):(length(fcst4.nn))]
fcst5.nn <- fcst5.nn[(length(returns.train1[,1])+1):(length(fcst5.nn))]
plot(fcst.nn,type="l",main="forecast")
plot(fcst1.nn,type="l",main="forecast1")
plot(fcst2.nn,type="l",main="forecast2")
plot(fcst3.nn,type="l",main="forecast3")
plot(fcst4.nn,type="l",main="forecast4")
plot(fcst5.nn,type="l",main="forecast5")

fe.nn <- returns.test[,1] - fcst.nn
fe1.nn <- returns.test[,1] - fcst1.nn
fe2.nn <- returns.test[,1] - fcst2.nn
fe3.nn <- returns.test[,1] - fcst3.nn
fe4.nn <- returns.test[,1] - fcst4.nn
fe5.nn <- returns.test[,1] - fcst5.nn


plot(fe.nn,type="l",,main="forecast er")
plot(fe1.nn,type="l",main="forecast1 er")
plot(fe2.nn,type="l",main="forecast2 er")
plot(fe3.nn,type="l",main="forecast3 er")
plot(fe4.nn,type="l",main="forecast4 er")
plot(fe5.nn,type="l",main="forecast5 er")
# scatter3D(fit$fitted,returns$GSPC.Adjusted,pch=18,cex=2,theta = 20,phi=20,surf = list(x=fit$fitted,y=returns$GSPC.Adjusted))
