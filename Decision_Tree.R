#AUthor: Kristen Bystrom
#Date: May 25th, 2018
# Purpose: create a decision tree for ASA competition
# http://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/

mydata <- read.csv("final_train_simplified.csv", stringsAsFactors = FALSE, na.strings = "?")
testdata <- read.csv("final_test_simplified.csv", stringsAsFactors = FALSE, na.strings = "?")

# if it reads in with random extra columns, run the following two lines:
mydata$X.1 <- NULL
mydata$X <- NULL
mydata$PrecipitationIn[mydata$PrecipitationIn == "T"] <- 0
mydata$PrecipitationIn <- 0

# Scale mydata
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
mydata[, 1:20] <- data.frame(lapply(mydata[, 1:20], scl))
mydata <- mydata[which(mydata$NewEvent != "Hail"),]
head(mydata)

# install.packages(c("rpart", "party", "ctree", "tree", "caret", "rpart.plot"))
library(rpart)
library(party)
library(tree)
library(caret)
library(beepr)
library(rpart.plot)
library(randomForest)
library(e1071)

anyNA(mydata)

#predictors <- data[,c("Max_TemperatureF",         "Mean_TemperatureF",        
#                "Min_TemperatureF",          "Max_Dew_PointF"            ,"MeanDew_PointF"            ,"Min_DewpointF"  ,          
#                "Max_Humidity"     ,         "Mean_Humidity"             ,"Min_Humidity"              ,"Max_Sea_Level_PressureIn" ,
#                "Mean_Sea_Level_PressureIn" ,"Min_Sea_Level_PressureIn"  ,"Max_VisibilityMiles"       ,"Mean_VisibilityMiles"     ,
#                "Min_VisibilityMiles"       ,"Max_Wind_SpeedMPH"         ,"Mean_Wind_SpeedMPH"        ,"Max_Gust_SpeedMPH"        ,
#                "PrecipitationIn"           ,"CloudCover"                ,"Events"                    ,"WindDirDegrees"           ,
#                "AirPtCd"                   ,"CountNA"                   ,"Fog"                       ,"Rain"                     ,
#                "Thunderstorm"              ,"Snow"                      ,"Tornado"                   ,"Hail"        )]

f <- as.formula(NewEvent ~ Max_TemperatureF + Mean_TemperatureF + Min_TemperatureF + Max_Dew_PointF + MeanDew_PointF + Min_DewpointF + Max_Humidity + Mean_Humidity + Min_Humidity + Max_Sea_Level_PressureIn + Mean_Sea_Level_PressureIn + Min_Sea_Level_PressureIn + Max_VisibilityMiles + Mean_VisibilityMiles + Min_VisibilityMiles + Max_Wind_SpeedMPH + Mean_Wind_SpeedMPH + Max_Gust_SpeedMPH  + CloudCover + WindDirDegrees)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(f,
                   data = mydata, 
                   method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

dtPrediction = predict(dtree_fit, newdata = testdata[, y_names])
dtPrediction <- factor(dtPrediction, ordered = FALSE)
testdata$NewEvent <- as.factor(testdata$NewEvent)
dtResults = myAccuracy(testdata$NewEvent, dtPrediction)

#plot ROC curve
testdata$ATornado <- ifelse(testdata$NewEvent == "Tornado", 1, 0)
testdata$PTornado <- ifelse(dtPrediction == "Tornado", 1, 0)

basicplot <- ggplot(testdata, aes(d = ATornado, m = PTornado, color = "test"))  + geom_roc(n.cut = 0)
basicplot + style_roc(xlab = "False Positive Rate", ylab = "True Positive Rate") + theme_dark() + ggtitle("ROC Curve") + annotate("text", x = .75, y = .25, label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2)))


plotResults(dtResults)
plotResults2(dtResults)
skrrrahh(sound = 26)
accuracy(testdata$NewEvent, dtPrediction)


accuracy = function(actual, predicted) {
  mean(actual == predicted)
}


#Random Forest
fN <- as.formula(NewEventNumeric ~ Max_TemperatureF + Mean_TemperatureF + Min_TemperatureF + Max_Dew_PointF + MeanDew_PointF + Min_DewpointF + Max_Humidity + Mean_Humidity + Min_Humidity + Max_Sea_Level_PressureIn + Mean_Sea_Level_PressureIn + Min_Sea_Level_PressureIn + Max_VisibilityMiles + Mean_VisibilityMiles + Min_VisibilityMiles + Max_Wind_SpeedMPH + Mean_Wind_SpeedMPH + Max_Gust_SpeedMPH  + CloudCover + WindDirDegrees)
y_names = c("Max_TemperatureF", "Mean_TemperatureF",  "Min_TemperatureF",  "Max_Dew_PointF",  "MeanDew_PointF", "Min_DewpointF", "Max_Humidity", "Mean_Humidity", "Min_Humidity", "Max_Sea_Level_PressureIn",  "Mean_Sea_Level_PressureIn",  "Min_Sea_Level_PressureIn",  "Max_VisibilityMiles", "Mean_VisibilityMiles", "Min_VisibilityMiles", "Max_Wind_SpeedMPH",  "Mean_Wind_SpeedMPH", "Max_Gust_SpeedMPH", "CloudCover", "WindDirDegrees") 
mydata[,y_names] = lapply(mydata[,y_names],as.numeric)
mydata$NewEvent <- factor(mydata$NewEvent, levels = c("No Event", "Fog", "Rain", "Snow", "Thunderstorm", "Tornado"), ordered = TRUE)
mydata$NewEventNumeric = as.numeric(mydata$NewEvent)
rfModel <- randomForest(f, data=mydata[,c(y_names, "NewEvent")])
rfPrediction = predict(rfModel, newdata = testdata[, y_names])
rfResults = myAccuracy(testdata$NewEvent, rfPrediction)
accuracy(testdata$NewEvent, prediction)
