#Author: Kristen BYstrom
#Date: June 21
#Purpose: train a model with a neural network

# FOllowed tutorial here:
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

require(neuralnet)
require(nnet)
library(randomForest)
require(ggplot2)
install.packages("reshape") 
library(reshape)
library(dplyr)
set.seed(10)

install.packages("devtools")
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


if(!require(devtools)) {install.packages(devtools)}
devtools::install_github("brooke-watson/BRRR")
library(BRRR)

#You can also change your options so DJ Kaled consoles you every time you hit an error message
options(error = function() {skrrrahh(34)})


mydata <- read.csv("final_train_simplified.csv", stringsAsFactors = FALSE, na.strings = "?")
testdata <- read.csv("final_test_simplified.csv", stringsAsFactors = FALSE, na.strings = "?")

# if it reads in with random extra columns, run the following two lines:
mydata$X.1 <- NULL
mydata$X <- NULL
mydata$PrecipitationIn[mydata$PrecipitationIn == "T"] <- 0
mydata$PrecipitationIn <- as.numeric(mydata$PrecipitationIn)


#Try plotting some of the variables
jitter <- position_jitter(width = 0.4, height = 0)
plt1 <- ggplot(mydata, aes(x = Max_VisibilityMiles, y = Max_Humidity, colour = as.factor(NewEvent))) +
  geom_point(position = jitter, alpha = 0.5) +
  ggtitle("Weather")
plt1

# Scale mydata
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
mydata[, 1:20] <- data.frame(lapply(mydata[, 1:20], scl))
mydata <- mydata[which(mydata$NewEvent != "Hail"),]
head(mydata)

# Fit neural net
f <- as.formula(NewEvent ~ Max_TemperatureF + Mean_TemperatureF + Min_TemperatureF + Max_Dew_PointF + MeanDew_PointF + Min_DewpointF + Max_Humidity + Mean_Humidity + Min_Humidity + Max_Sea_Level_PressureIn + Mean_Sea_Level_PressureIn + Min_Sea_Level_PressureIn + Max_VisibilityMiles + Mean_VisibilityMiles + Min_VisibilityMiles + Max_Wind_SpeedMPH + Mean_Wind_SpeedMPH + Max_Gust_SpeedMPH  + CloudCover + WindDirDegrees)

y_names = c("Max_TemperatureF", "Mean_TemperatureF",  "Min_TemperatureF",  "Max_Dew_PointF",  "MeanDew_PointF", "Min_DewpointF", "Max_Humidity", "Mean_Humidity", "Min_Humidity", "Max_Sea_Level_PressureIn",  "Mean_Sea_Level_PressureIn",  "Min_Sea_Level_PressureIn",  "Max_VisibilityMiles", "Mean_VisibilityMiles", "Min_VisibilityMiles", "Max_Wind_SpeedMPH",  "Mean_Wind_SpeedMPH", "Max_Gust_SpeedMPH", "CloudCover", "WindDirDegrees") 
mydata[,y_names] = lapply(mydata[,y_names],as.numeric)
mydata$NewEvent <- factor(mydata$NewEvent, levels = c("No Event", "Fog", "Rain", "Hail", "Snow", "Thunderstorm", "Tornado"), ordered = TRUE)
nn <- nnet(f,data=mydata[,c(y_names, "NewEvent")],size = 20, na.action = na.omit)

nnPred <- (predict(nnet(f,data=mydata[,c(y_names, "NewEvent")],size = 13, na.action = na.omit), testdata[, y_names]))
results = myAccuracy(testdata$NewEvent, recode_factor(max.col(nnPred), '1' = "No Event", '2' = "Fog", '3' = "Rain", '4' = "Snow", '5' = "Thunderstom", '6' = "Tornado"))$Accuracy
plotResults(nnResults)
plotResults2(nnResults)
head(testdata)

a <- data.frame(recode_factor(max.col(nnPred), '0' = "No Event", '0' = "Fog", '0' = "Rain", '0' = "Snow", '0' = "Thunderstom", '1' = "Tornado"))
a$ATornado <- ifelse(testdata$NewEvent == "Tornado", 1, 0)
a$PTornado <- ifelse(a == "Tornado", 1, 0)
a[is.na(a$PTornado),] <- 0


#plot ROC curve
basicplot <- ggplot(a, aes(d = ATornado, m = PTornado, color = "test"))  + geom_roc(n.cut = 0)
library(ggthemes)  
basicplot + style_roc(xlab = "False Positive Rate", ylab = "True Positive Rate") + theme_dark() + ggtitle("ROC Curve") + annotate("text", x = .75, y = .25, label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2)))
  

#plot the model
plot.nnet(nn)

#parameter tuning
accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

set.seed(42)
size_to_try <- 1:20
acc_k <- rep(x = 0, times = length(size_to_try))
acc_each <- matrix(nrow = length(size_to_try), ncol = 6)

for(i in seq_along(size_to_try)){
  pred <- (predict(nnet(f,data=mydata[,c(y_names, "NewEvent")],size = size_to_try[i], na.action = na.omit), testdata[, y_names]))
  
  acc_k[i] <- accuracy(testdata$NewEvent, recode_factor(max.col(pred), '1' = "No Event", '2' = "Fog", '3' = "Rain",  '4' = "Snow", '5' = "Thunderstom", '6' = "Tornado"))
  
  acc_each[i,] <- myAccuracy(testdata$NewEvent, recode_factor(max.col(pred), '1' = "No Event", '2' = "Fog", '3' = "Rain", '4' = "Snow", '5' = "Thunderstom", '6' = "Tornado"))$Accuracy
}

#to choose k
plot(y = acc_each[,1],x = size_to_try, type = "l", col = "blue", ylim = range(0:1), xlim = range(size_to_try))
lines(y = acc_each[,2],x = size_to_try, type = "l", col = "brown")
lines(y = acc_each[,3],x = size_to_try, type = "l", col = "cyan")
lines(y = acc_each[,4],x = size_to_try, type = "l", col = "darkgreen")
lines(y = acc_each[,5],x = size_to_try, type = "l", col = "dark orange")
lines(y = acc_each[,6],x = size_to_try, type = "l", col = "red")
axis(side = 1, at=size_to_try)

#Add a legend
legend(1, 0.8, legend=c("No Event", "Fog", "Rain", "Snow", "Thunderstorm", "Tornado"),
       col=c("blue", "brown", "cyan", "darkgreen", "dark orange", "red"),lty=1, cex=0.8)

