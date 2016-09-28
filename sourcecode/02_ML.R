# Run this file after runing Stats.R
attach(new.data)
my.data <- new.data[,-c(1)]
my.data <- my.data[,-c(25:33)]
my.data <- my.data[ !apply(is.na(my.data), 1,any), ]
my.train <- my.data[1:68,]
test.data <- my.data[69:74,]
min.val=0
max.val=48
seed.val <- 1280
library(Rrdrand)
set.seed(seed.val, kind="Mersenne-Twister")
mae <- function(error)
{
  mean(abs(error))
}
rmse <- function(error)
{
  sqrt(mean(error^2))
}
precision.rmse <- function(error)
{
  rmse=(1-(rmse(error)/max.val))*100
}
precision.sd <- function(error)
{
  (sd=(sd(error)/max.val)*100)
}
precision.mae <- function(error)
{
  mae=(1-(mae(error)/max.val))*100
}
accuracy.rmse <- function(error)
{
  rmse=(1-rmse(error))*100
}
accuracy.sd <- function(error)
{
  (sd=(sd(error))*100)
}
accuracy.mae <- function(error)
{
  mae=(1-mae(error))*100
}
pred.model <- (ALSFRSr~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21)
library(randomForest)
rfmodel <- randomForest(pred.model, data=my.train,importance=TRUE,ntree=2000,na.action=na.omit)
rfprediction <- predict(rfmodel,my.data)
rfprediction <- lapply(rfprediction, function(x) ifelse(x > max.val, max.val, x))
rfprediction <- lapply(rfprediction, function(x) ifelse(x<min.val,min.val,x))
rfprediction <- as.numeric(rfprediction)
my.data$rf.prediction <- as.numeric(rfprediction)
str(my.data)
rferror <- my.data$ALSFRSr - my.data$rf.prediction
rfpredictionMAE <- mae(rferror)
rfpredictionRMSE <- rmse(rferror)
fit<- rfmodel
svg("Error rate vs random seeds.svg")
plot(fit,main="Random Forest: Error Rate vs Number of Trees", type = "h", col = "blue", lwd = 10)
windowsFonts(A=windowsFont("Helvetica"))
dev.off()
imp=importance(fit)
imp.ma=as.matrix(imp)
imp.df=data.frame(imp.ma)
write.csv(imp.df, "imp.df.csv", row.names=TRUE)
  imp.df.csv=read.csv("imp.df.csv",header=TRUE)
library(ggthemes)
library(gridExtra)
library(grid)
library(caret)
colnames(imp.df.csv)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),]
imp.sort = transform(imp.df.csv, Variable = reorder(Variable, MeanDecreaseAccuracy))
VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) +
            ylab("Mean Decrease Accuracy")+
            xlab("")+
            geom_bar (stat="identity",fill="firebrick",alpha=.8,width=.75)+
            coord_flip()+
            theme_few()+
            theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=16),
                  axis.text.y  = element_text(angle=21, vjust=0.5, size=16))
imp.sort.Gini <- transform(imp.df.csv, Variable = reorder(Variable, MeanDecreaseGini))
VIP.Gini=ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) +
                ylab("Mean Decrease Gini")+
                xlab("")+
                geom_bar (stat="identity",fill="skyblue",alpha=.8,width=.75)+
                coord_flip()+
                theme_few()+
                theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=16),
                axis.text.y  = element_text(angle=21, vjust=0.5, size=16))
svg("VIP_Plot.svg")
  VarImpPlot.VIP=arrangeGrob(VIP)
  grid.draw(VarImpPlot.VIP)
dev.off()
svg("GINI_Plot.svg")
  VarImpPlot.Gini=arrangeGrob(VIP.Gini)
  grid.draw(VarImpPlot.Gini)
dev.off()
p <- aggregate(rf.prediction ~ ID, data=my.data, FUN=function(x) c(NA,diff(x)))
my.data$rfprecision.error <- (c(t(p[-1]), recursive=TRUE))
str(my.data)
rfprecision.error <- data.frame(ID = my.data$ID, rfprecision.error = my.data$rfprecision.error)
rfprecision.error <- na.omit(rfprecision.error)
my.data$rfaccuracy.error <- my.data$ALSFRSr - my.data$rf.prediction
rfaccuracy.error <- data.frame(ID = my.data$ID, rfaccuracy.error = my.data$rfaccuracy.error,ALSFRSr=my.data$ALSFRSr)
rfaccuracy.error <- na.omit(rfaccuracy.error)
  percentage.rfaccuracy.SD = accuracy.sd(rfaccuracy.error$rfaccuracy.error/rfaccuracy.error$ALSFRSr)
  percentage.rfaccuracy.MAE= accuracy.mae(rfaccuracy.error$rfaccuracy.error/rfaccuracy.error$ALSFRSr)
  percentage.rfaccuracy.RMSE= accuracy.rmse(rfaccuracy.error$rfaccuracy.error/rfaccuracy.error$ALSFRSr)
  percentage.rfprecision.SD = precision.sd(rfprecision.error$rfprecision.error)
  percentage.rfprecision.MAE= precision.mae(rfprecision.error$rfprecision.error)
  percentage.rfprecision.RMSE = precision.rmse(rfprecision.error$rfprecision.error)
rf.test.prediction <- predict(rfmodel,test.data)
rf.test.prediction <- lapply(rf.test.prediction, function(x) ifelse(x > max.val, max.val, x))
rf.test.prediction <- lapply(rf.test.prediction, function(x) ifelse(x<min.val,min.val,x))
rf.test.prediction <- as.numeric(rf.test.prediction)
test.data$rf.prediction <- as.numeric(rf.test.prediction)
rfpb <- aggregate(rf.prediction ~ ID, data=test.data, FUN=function(x) c(NA,diff(x)))
test.data$rfprecision.error <- (c(t(rfpb[-1]), recursive=TRUE))
test.rfprecision.error <- data.frame(ID = test.data$ID, rfprecision.error = test.data$rfprecision.error, ALSFRSr=test.data$ALSFRSr)
test.rfprecision.error <- na.omit(test.rfprecision.error)
  test.percentage.rfprecision.SD = precision.sd(test.rfprecision.error$rfprecision.error)
  test.percentage.rfprecision.RMSE=precision.rmse(test.rfprecision.error$rfprecision.error)
  test.percentage.rfprecision.MAE=precision.mae(test.rfprecision.error$rfprecision.error)
test.data$rfaccuracy.error <- test.data$ALSFRSr - test.data$rf.prediction
test.rfaccuracy.error <- data.frame(ID = test.data$ID, rfaccuracy.error = test.data$rfaccuracy.error,ALSFRSr=test.data$ALSFRSr)
  test.percentage.rfaccuracy.SD = accuracy.sd(test.rfaccuracy.error$rfaccuracy.error/test.rfaccuracy.error$ALSFRSr)
  test.percentage.rfaccuracy.RMSE=accuracy.rmse(test.rfaccuracy.error$rfaccuracy.error/test.rfaccuracy.error$ALSFRSr)
  test.percentage.rfaccuracy.MAE=accuracy.mae(test.rfaccuracy.error$rfaccuracy.error/test.rfaccuracy.error$ALSFRSr)
test.metrics1 <- data.frame(ID=c("Overall_ML"), accuracy.MAE=percentage.rfaccuracy.MAE,
                            precison.MAE=percentage.rfprecision.MAE,
                            accuracy.RMSE=percentage.rfaccuracy.RMSE,
                            precison.RMSE=percentage.rfprecision.RMSE,
                            accuracy.SD=percentage.rfaccuracy.SD,
                            precison.SD=percentage.rfprecision.SD)
test.metrics2 <- data.frame(ID=c("Blinded_test_ML"), accuracy.MAE=test.percentage.rfaccuracy.MAE,
                            precison.MAE=test.percentage.rfprecision.MAE,
                            accuracy.RMSE=test.percentage.rfaccuracy.RMSE,
                            precison.RMSE=test.percentage.rfprecision.RMSE,
                            accuracy.SD=test.percentage.rfaccuracy.SD,
                            precison.SD=test.percentage.rfprecision.SD)
lmmodel <- lm(pred.model, data=my.train)
lmprediction.train<- predict(lmmodel, data=my.train)
lmprediction.train <- lapply(lmprediction.train, function(x) ifelse(x > max.val, max.val, x))
lmprediction.train <- lapply(lmprediction.train, function(x) ifelse(x<min.val,min.val,x))
my.train$lm.prediction <- as.numeric(lmprediction.train)
lmprediction.test <- predict(lmmodel,test.data)
lmprediction.test <- lapply(lmprediction.test, function(x) ifelse(x > max.val, max.val, x))
lmprediction.test <- lapply(lmprediction.test, function(x) ifelse(x<min.val,min.val,x))
test.data$lm.prediction <- as.numeric(lmprediction.test)
my.train.lm <- data.frame(ID=my.train$ID, lm.prediction=my.train$lm.prediction)
test.data.lm <- data.frame(ID=test.data$ID, lm.prediction=test.data$lm.prediction)
lmdata <- rbind(my.train.lm,test.data.lm)
my.data$lm.prediction <- as.numeric(lmdata$lm.prediction)
lmerror <- my.data$ALSFRSr - my.data$lm.prediction
lmp <- aggregate(lm.prediction ~ ID, data=my.data, FUN=function(x) c(NA,diff(x)))
my.data$lmprecision.error <- (c(t(lmp[-1]), recursive=TRUE))
str(my.data)
lmprecision.error <- data.frame(ID = my.data$ID, lmprecision.error = my.data$lmprecision.error)
lmprecision.error <- na.omit(lmprecision.error)
  percentage.lmprecision.SD = precision.sd(lmprecision.error$lmprecision.error)
  percentage.lmprecision.RMSE=precision.rmse(lmprecision.error$lmprecision.error)
  percentage.lmprecision.MAE=precision.mae(lmprecision.error$lmprecision.error)
my.data$lmaccuracy.error <- my.data$ALSFRSr - my.data$lm.prediction
lmaccuracy.error <- data.frame(ID = my.data$ID, lmaccuracy.error = my.data$lmaccuracy.error, ALSFRSr=my.data$ALSFRSr)
lmaccuracy.error <- na.omit(lmaccuracy.error)
  percentage.lmaccuracy.SD = accuracy.sd(lmaccuracy.error$lmaccuracy.error/lmaccuracy.error$ALSFRSr)
  percentage.lmaccuracy.RMSE=accuracy.rmse(lmaccuracy.error$lmaccuracy.error/lmaccuracy.error$ALSFRSr)
  percentage.lmaccuracy.MAE=accuracy.mae(lmaccuracy.error$lmaccuracy.error/lmaccuracy.error$ALSFRSr)
lmpb <- aggregate(lm.prediction ~ ID, data=test.data, FUN=function(x) c(NA,diff(x)))
test.data$lmprecision.error <- (c(t(lmpb[-1]), recursive=TRUE))
test.lmprecision.error <- data.frame(ID = test.data$ID, lmprecision.error = test.data$lmprecision.error)
test.lmprecision.error <- na.omit(test.lmprecision.error)
  test.percentage.lmprecision.SD = precision.sd(test.lmprecision.error$lmprecision.error)
  test.percentage.lmprecision.RMSE=precision.rmse(test.lmprecision.error$lmprecision.error)
  test.percentage.lmprecision.MAE=precision.mae(test.lmprecision.error$lmprecision.error)
test.data$lmaccuracy.error <- test.data$ALSFRSr - test.data$lm.prediction
test.lmaccuracy.error <- data.frame(ID = test.data$ID, lmaccuracy.error = test.data$lmaccuracy.error, ALSFRSr = test.data$ALSFRSr)
  test.percentage.lmaccuracy.SD = accuracy.sd(test.lmaccuracy.error$lmaccuracy.error/test.lmaccuracy.error$ALSFRSr)
  test.percentage.lmaccuracy.RMSE=accuracy.rmse(test.lmaccuracy.error$lmaccuracy.error/test.lmaccuracy.error$ALSFRSr)
  test.percentage.lmaccuracy.MAE=accuracy.mae(test.lmaccuracy.error$lmaccuracy.error/test.lmaccuracy.error$ALSFRSr)
test.metrics3 <- data.frame(ID=c("Overall_MLR"),accuracy.MAE=percentage.lmaccuracy.MAE,
                            precison.MAE=percentage.lmprecision.MAE,
                            accuracy.RMSE=percentage.lmaccuracy.RMSE,
                            precison.RMSE=percentage.lmprecision.RMSE,
                            accuracy.SD=percentage.lmaccuracy.SD,
                            precison.SD=percentage.lmprecision.SD)
test.metrics4 <- data.frame(ID=c("Blinded_test_MLR"),accuracy.MAE=test.percentage.lmaccuracy.MAE,
                            precison.MAE=test.percentage.lmprecision.MAE,
                            accuracy.RMSE=test.percentage.lmaccuracy.RMSE,
                            precison.RMSE=test.percentage.lmprecision.RMSE,
                            accuracy.SD=test.percentage.lmaccuracy.SD,
                            precison.SD=test.percentage.lmprecision.SD)
plotErrors <- function(errors1, type1, errors2, type2)
{
  # Histogram for prediction errors:
  error1binsize <- IQR(errors1)/4
  error1sd   <- sd(errors1)
  error1min  <- min(errors1) - error1sd*5
  error1max  <- max(errors1) + error1sd*3
  error2binsize <- IQR(errors2)/4
  error2sd   <- sd(errors2)
  error2min  <- min(errors2) - error2sd*5
  error2max  <- max(errors2) + error2sd*3
  # Generate normally distributed data with mean = 0 and standard deviation of the error data
  error1norm <- rnorm(10000, mean=0, sd=error1sd)
  error1min2 <- min(error1norm)
  error1max2 <- max(error1norm)
  if (error1min2 < error1min) { error1min <- error1min2 }
  if (error1max2 > error1max) { error1max <- error1max2 }
  ifelse (type1 == 1, hist.color1 <- rgb(1,0,0,1/4),
          ifelse(type1 == 2, hist.color1 <- rgb(0,0,1,1/4), hist.color1 <- "grey"))
  error1bins <- seq(error1min, error1max, error1binsize)
  error2norm <- rnorm(10000, mean=0, sd=error2sd)
  error2min2 <- min(error2norm)
  error2max2 <- max(error2norm)
  if (error2min2 < error2min) { error2min <- error2min2 }
  if (error2max2 > error2max) { error2max <- error2max2 }
  ifelse (type2 == 1, hist.color2 <- rgb(1,0,0,1/4),
          ifelse(type2 == 2, hist.color2 <- rgb(0,0,1,1/4), hist.color2 <- "grey"))
  error2bins <- seq(error2min, error2max, error2binsize)
  # Generate normally distributed data with mean 0 and error data SD
  error1hist <- hist(error1norm, plot=FALSE, breaks=error1bins)
  error2hist <- hist(error2norm, plot=FALSE, breaks=error2bins)
  # Histograms with the area under the plot = 1
  hist(errors1, freq=FALSE, col=hist.color1, breaks=error1bins, ylim=c(0,1), main="", xlab="", ylab="")
  hist(errors2, freq=FALSE, col=hist.color2, breaks=error2bins, ylim=c(0,1), main="", xlab="", ylab="", add=T)
  # Plots the normal curve as a line overlay
  points(error1hist$mids, error1hist$density, type="l", col="darkblue", lwd=2)
  points(error2hist$mids, error2hist$density, type="l", col="firebrick", lwd=2)
  # Add legends
  legend("topright", inset=0,
         c("Accuracy error","Precision error"), fill=c(hist.color1, hist.color2), horiz=TRUE)
}
svg("ALSFRSr vs. TST.svg")
  plot(my.data$V4,my.data$ALSFRSr, xlab="TST amplitude ratio", ylab="ALSFRSr", col = "blue", pch=16,cex=2)
  points(my.data$V4, my.data$rf.prediction, col = "red", pch=16,cex=2)
  legend("bottomright", inset=0,
       c("ALSFRSr","Predicted"), fill=c("blue","red"), horiz=TRUE)
      lines(lowess(my.data$ALSFRSr~my.data$V4), col="blue",lwd=2,lty=1)
      lines(lowess(my.data$rf.prediction~my.data$V4), col="red",lwd=2,lty=1)
      abline(lm(my.data$ALSFRSr~my.data$V4), col="blue",lwd=2,lty=4)
      abline(lm(my.data$rf.prediction~my.data$V4), col="red",lwd=2,lty=4)
dev.off()
sink("Prediction model summary.txt")
if(hasRDRAND())
  print(RNGkind())
  rfmodel
  summary(rfmodel)
  lmmodel
  summary(lmmodel)
sink()
test.metrics <- rbind(test.metrics1,test.metrics2,test.metrics3,test.metrics4)
write.csv(test.metrics,"test.metrics.csv",row.names=FALSE)
write.csv(my.data,"my.data.csv",row.names=FALSE)
str(my.data)
library(plyr)
library(ggplot2)
overall.accuracy.error.rf <- data.frame(ID=my.data$ID, Model="Overall ML accuracy error",Type="Accuracy",Error=abs((my.data$rfaccuracy.error/my.data$ALSFRSr)*100))
blindtest.accuracy.error.rf <- data.frame(ID=test.data$ID,Model="Blinded test ML accuracy error",Type="Accuracy",Error=abs((test.data$rfaccuracy.error/test.data$ALSFRSr)*100))
overall.precision.error.rf <- data.frame(ID=my.data$ID, Model="Overall ML precision error",Type="Precision",Error=abs((my.data$rfprecision.error/48)*100))
blindtest.precision.error.rf <- data.frame(ID=test.data$ID,Model="Blinded test ML precision error",Type="Precision",Error=abs((test.data$rfprecision.error/48)*100))
overall.accuracy.error.lm <- data.frame(ID=my.data$ID, Model="Overall MLR accuracy error",Type="Accuracy",Error=abs((my.data$lmaccuracy.error/my.data$ALSFRSr)*100))
blindtest.accuracy.error.lm <- data.frame(ID=test.data$ID,Model="Blinded test MLR accuracy error",Type="Accuracy",Error=abs((test.data$lmaccuracy.error/test.data$ALSFRSr)*100))
overall.precision.error.lm <- data.frame(ID=my.data$ID, Model="Overall MLR precision error",Type="Precision",Error=abs((my.data$lmprecision.error/48)*100))
blindtest.precision.error.lm <- data.frame(ID=test.data$ID,Model="Blinded test MLR precision error",Type="Precision",Error=abs((test.data$lmprecision.error/48)*100))
bg.data <- rbind(overall.accuracy.error.rf,
            blindtest.accuracy.error.rf,
            overall.precision.error.rf,
            blindtest.precision.error.rf,
            overall.accuracy.error.lm,
            blindtest.accuracy.error.lm,
            overall.precision.error.lm,
            blindtest.precision.error.lm)
bg.data <- bg.data[ !apply(is.na(my.data), 1,any), ]
data <- bg.data
stderr <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
lowsd <- function(x){return(mean(x)-stderr(x))}
highsd <- function(x){return(mean(x)+stderr(x))}
svg("Accuracy and precision error.svg")
  ggplot(bg.data,aes(Type,Error,fill=Model))+
  stat_summary(fun.y=mean, geom="bar", position="dodge", colour='white')+
  stat_summary(fun.y=mean, fun.ymin=lowsd, fun.ymax=highsd,
               geom="errorbar", position="dodge",color = 'black', size=.5)
dev.off()
svg("Error_distribution.svg", width=13,height=8)
plotErrors(((my.data$rfaccuracy.error/my.data$ALSFRSr)*100), 1, (na.omit((my.data$rfprecision.error/48)*100)), 2)
dev.off()
