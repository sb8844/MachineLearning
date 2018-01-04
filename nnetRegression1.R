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
returns$Lag1 <- lag(returns$TSLA.Adjusted)
returns$Lag2 <- lag(returns$TSLA.Adjusted,2)
returns$Lag3 <- lag(returns$TSLA.Adjusted,3)
returns <- na.omit(returns)
returns.train <- returns["2010-09-09/2016-01-27"]
returns.test <- returns["2016-01-28/2017-11-12"]

returns.train0 <- cbind(returns.train[,2],returns.train[,3],returns.train[,4],returns.train[,5])
returns.train1 <- cbind(returns.train[,2],returns.train[,3],returns.train[,4])
returns.train2 <- cbind(returns.train[,2],returns.train[,3])
returns.train3 <- cbind(returns.train[,2])
returns.train4 <- cbind(returns.train[,3],returns.train[,4],returns.train[,5])
returns.train5 <- cbind(returns.train[,3],returns.train[,4])

returns.test0 <- cbind(returns.test[,2],returns.test[,3],returns.test[,4],returns.test[,5])
returns.test1 <- cbind(returns.test[,2],returns.test[,3],returns.test[,4])
returns.test2 <- cbind(returns.test[,2],returns.test[,3])
returns.test3 <- cbind(returns.test[,2])
returns.test4 <- cbind(returns.test[,3],returns.test[,4],returns.test[,5])
returns.test5 <- cbind(returns.test[,3],returns.test[,4])

# NO HIDDEN LAYERS
fit.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train0$GSPC.Adjusted + returns.train0$Lag1 +returns.train0$Lag2 + returns.train0$Lag3,data = returns.train0,hidden=50, err.fct="sse",rep = 100, linear.output = T)

fit1.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train1$GSPC.Adjusted + returns.train1$Lag1 +returns.train1$Lag2,data = returns.train1, hidden=0,
                     rep=10, err.fct="sse",linear.output = T)
fit2.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train2$GSPC.Adjusted + returns.train2$Lag1,data = returns.train2, hidden=0,
                     rep=10, err.fct="sse",linear.output = T)
fit3.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train3$GSPC.Adjusted ,data = returns.train3, hidden=0,
                     rep=10, err.fct="sse",linear.output = T)
fit4.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train4$Lag1 +returns.train4$Lag2 + returns.train4$Lag3,data = returns.train4, hidden=0,
                     rep=10, err.fct="sse",linear.output = T)
fit5.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train5$Lag1 +returns.train5$Lag2,data = returns.train5, hidden=0,
                     rep=10, err.fct="sse",linear.output = T)

# HIDDEN LAYERS (1)
fit6.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train0$GSPC.Adjusted + returns.train0$Lag1 +returns.train0$Lag2 + returns.train0$Lag3, returns.train0, hidden=1,
                     rep=10, err.fct="sse",linear.output = T)
fit7.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train1$GSPC.Adjusted + returns.train1$Lag1 +returns.train1$Lag2,data = returns.train1, hidden=1,
                     rep=10, err.fct="sse",linear.output = T)
fit8.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train2$GSPC.Adjusted + returns.train2$Lag1,data = returns.train2,  hidden=1,
                     rep=10, err.fct="sse",linear.output = T)
fit9.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train3$GSPC.Adjusted , data = returns.train3, hidden=1,
                     rep=10, err.fct="sse",linear.output = T)
fit10.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train4$Lag1 +returns.train4$Lag2 + returns.train4$Lag3, data = returns.train4, hidden=1,
                      rep=10, err.fct="sse",linear.output = T)
fit11.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train5$Lag1 +returns.train5$Lag2,data = returns.train5, hidden=1,
                      rep=10, err.fct="sse",linear.output = T)

# HIDDEN LAYERS (3)
fit12.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train0$GSPC.Adjusted + returns.train0$Lag1 +returns.train0$Lag2 + returns.train0$Lag3, returns.train0, hidden=3,
                      rep=10, err.fct="sse",linear.output = T)
fit13.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train1$GSPC.Adjusted + returns.train1$Lag1 +returns.train1$Lag2,data = returns.train1,hidden=3,rep=10, err.fct="sse", linear.output = T)
fit14.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train2$GSPC.Adjusted + returns.train2$Lag1,data = returns.train2, hidden=3,rep=10, err.fct="sse",linear.output = T)
fit15.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train3$GSPC.Adjusted ,data = returns.train3, hidden=3,rep=10, err.fct="sse",linear.output = T)
fit16.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train4$Lag1 +returns.train4$Lag2 + returns.train4$Lag3, data = returns.train4,hidden=3,rep=10, err.fct="sse",linear.output = T)
fit17.nn <- neuralnet(returns.train$TSLA.Adjusted ~ returns.train5$Lag1 +returns.train5$Lag2,data = returns.train5,hidden=3,rep=10, err.fct="sse",linear.output = T)


# ftable(predict(fit),returns$TSLA.Adjusted)
fcst.nn <- compute(fit.nn,returns.test0, rep = which.min(fit.nn$result.matrix[1,]))
fcst1.nn <- compute(fit1.nn,covariate = returns.test1)
fcst2.nn <- compute(fit2.nn,covariate = returns.test2)
fcst3.nn <- compute(fit3.nn,covariate = returns.test3)
fcst4.nn <- compute(fit4.nn,covariate = returns.test4)
fcst5.nn <- compute(fit5.nn,covariate = returns.test5)
fcst6.nn <- compute(fit6.nn,returns.test0)
fcst7.nn <- compute(fit7.nn,covariate = returns.test1)
fcst8.nn <- compute(fit8.nn,covariate = returns.test2)
fcst9.nn <- compute(fit9.nn,covariate = returns.test3)
fcst10.nn <- compute(fit10.nn,covariate = returns.test4)
fcst11.nn <- compute(fit11.nn,covariate = returns.test5)
fcst12.nn <- compute(fit12.nn,returns.test0)
fcst13.nn <- compute(fit13.nn,covariate = returns.test1)
fcst14.nn <- compute(fit14.nn,covariate = returns.test2)
fcst15.nn <- compute(fit15.nn,covariate = returns.test3)
fcst16.nn <- compute(fit16.nn,covariate = returns.test4)
fcst17.nn <- compute(fit17.nn,covariate = returns.test5)






# fcst.nn <- fcst.nn[(length(returns.train0[,1])+1):(length(fcst.nn))]
# fcst1.nn <- fcst1.nn[(length(returns.train1[,1])+1):(length(fcst1.nn))]
# fcst2.nn <- fcst2.nn[(length(returns.train2[,1])+1):(length(fcst2.nn))]
# fcst3.nn <- fcst3.nn[(length(returns.train3[,1])+1):(length(fcst3.nn))]
# fcst4.nn <- fcst4.nn[(length(returns.train4[,1])+1):(length(fcst4.nn))]
# fcst5.nn <- fcst5.nn[(length(returns.train5[,1])+1):(length(fcst5.nn))]




plot(fit.nn,rep="best",main="fit.nn")
plot(fit1.nn,rep="best",main="fit1.nn")
plot(fit2.nn,rep="best",main="fit2.nn")
plot(fit3.nn,rep="best",main="fit3.nn")
plot(fit4.nn,rep="best",main="fit4.nn")
plot(fit5.nn,rep="best",main="fit5.nn")
plot(fit6.nn,rep="best",main="fit6.nn")
plot(fit7.nn,rep="best",main="fit8.nn")
plot(fit8.nn,rep="best",main="fit9.nn")
plot(fit9.nn,rep="best",main="fit10.nn")
plot(fit10.nn,rep="best",main="fit11.nn")
plot(fit11.nn,rep="best",main="fit12.nn")
plot(fit12.nn,rep="best",main="fit12.nn")
plot(fit13.nn,rep="best",main="fit13.nn")
plot(fit14.nn,rep="best",main="fit14.nn")
plot(fit15.nn,rep="best",main="fit15.nn")
plot(fit16.nn,rep="best",main="fit16.nn")
plot(fit17.nn,rep="best",main="fit17.nn")






fe.nn <- returns.test0[,1] - fcst.nn$net.result
fe1.nn <- returns.test1[,1] - fcst1.nn$net.result
fe2.nn <- returns.test2[,1] - fcst2.nn$net.result
fe3.nn <- returns.test3[,1] - fcst3.nn$net.result
fe4.nn <- returns.test4[,1] - fcst4.nn$net.result
fe5.nn <- returns.test5[,1] - fcst5.nn$net.result
fe6.nn <- returns.test0[,1] - fcst.nn$net.result
fe7.nn <- returns.test1[,1] - fcst1.nn$net.result
fe8.nn <- returns.test2[,1] - fcst2.nn$net.result
fe9.nn <- returns.test3[,1] - fcst3.nn$net.result
fe10.nn <- returns.test4[,1] - fcst4.nn$net.result
fe11.nn <- returns.test5[,1] - fcst5.nn$net.result
fe12.nn <- returns.test0[,1] - fcst.nn$net.result
fe13.nn <- returns.test1[,1] - fcst1.nn$net.result
fe14.nn <- returns.test2[,1] - fcst2.nn$net.result
fe15.nn <- returns.test3[,1] - fcst3.nn$net.result
fe16.nn <- returns.test4[,1] - fcst4.nn$net.result
fe17.nn <- returns.test5[,1] - fcst5.nn$net.result


plot(fe.nn,type="l")
plot(fe1.nn,type="l")
plot(fe2.nn,type="l")
plot(fe3.nn,type="l")
plot(fe4.nn,type="l")
plot(fe5.nn,type="l")
plot(fe6.nn,type="l")
plot(fe7.nn,type="l")
plot(fe8.nn,type="l")
plot(fe9.nn,type="l")
plot(fe10.nn,type="l")
plot(fe11.nn,type="l")
plot(fe12.nn,type="l")
plot(fe13.nn,type="l")
plot(fe14.nn,type="l")
plot(fe15.nn,type="l")
plot(fe16.nn,type="l")
plot(fe17.nn,type="l")

# scatter3D(fit$fitted,returns$GSPC.Adjusted,pch=18,cex=2,theta = 20,phi=20,surf = list(x=fit$fitted,y=returns$GSPC.Adjusted))
