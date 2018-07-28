#Author: Kristen Bystrom
#Date: May 6, 2018
#Purpose: create function that outputs a nice data frame of accurcy and type 1 error results
#Inputs: takes two columns as inputs only where the events are one of ('Fog', 'No Event', 'Rain', 'Snow', 'Thunderstorm', 'Tornado')

library(dplyr)
library(lazyeval)

myAccuracy <- function(actual, predicted){
  #create dataframe
  myTest = data.frame(actual, predicted)
  myTest$actual = factor(myTest$actual, levels= c('Fog', 'No Event', 'Rain', 'Snow', 'Thunderstorm', 'Tornado'))
  myTest$predicted = factor(myTest$predicted, levels= c('Fog', 'No Event', 'Rain', 'Snow', 'Thunderstorm', 'Tornado'))
  myTest$correct = (myTest$actual == myTest$predicted)
  
  #create dataframe for results
  results = data.frame(rep(NA, 6), rep(NA, 6), rep(NA, 6), rep(NA, 6), rep(NA, 6), rep(NA, 6))
  colnames(results) = c("Accuracy", "True Positives", "False Positives", "False Positives Odds", "Count", "Event")
  rownames(results) = c('Fog', 'No Event', 'Rain', 'Snow', 'Thunderstorm', 'Tornado')
  
  #Get Accuracy
  results["Fog", "Accuracy"] = length(myTest[which((myTest$actual == "Fog") & (myTest$predicted == "Fog")),]$actual) / length(myTest[which(myTest$actual == "Fog"),]$actual)
  results["No Event", "Accuracy"] = length(myTest[which((myTest$actual == "No Event") & (myTest$predicted == "No Event")),]$actual) / length(myTest[which(myTest$actual == "No Event"),]$actual)
  results["Rain", "Accuracy"] = length(myTest[which((myTest$actual == "Rain") & (myTest$predicted == "Rain")),]$actual) / length(myTest[which(myTest$actual == "Rain"),]$actual)
  results["Snow", "Accuracy"] = length(myTest[which((myTest$actual == "Snow") & (myTest$predicted == "Snow")),]$actual) / length(myTest[which(myTest$actual == "Snow"),]$actual)
  results["Thunderstorm", "Accuracy"] = length(myTest[which((myTest$actual == "Thunderstorm") & (myTest$predicted == "Thunderstorm")),]$actual) / length(myTest[which(myTest$actual == "Thunderstorm"),]$actual)
  results["Tornado", "Accuracy"] = length(myTest[which((myTest$actual == "Tornado") & (myTest$predicted == "Tornado")),]$actual) / length(myTest[which(myTest$actual == "Tornado"),]$actual)

  #Get True Positives
  results["Fog", "True Positives"] = length(myTest[which((myTest$actual == "Fog") & (myTest$predicted == "Fog")),]$actual)
  results["No Event", "True Positives"] = length(myTest[which((myTest$actual == "No Event") & (myTest$predicted == "No Event")),]$actual)
  results["Rain", "True Positives"] = length(myTest[which((myTest$actual == "Rain") & (myTest$predicted == "Rain")),]$actual)
  results["Snow", "True Positives"] = length(myTest[which((myTest$actual == "Snow") & (myTest$predicted == "Snow")),]$actual)
  results["Thunderstorm", "True Positives"] = length(myTest[which((myTest$actual == "Thunderstorm") & (myTest$predicted == "Thunderstorm")),]$actual)
  results["Tornado", "True Positives"] = length(myTest[which((myTest$actual == "Tornado") & (myTest$predicted == "Tornado")),]$actual)
  
  
  #Get False Positives
  results["Fog", "False Positives"] = (length(myTest[which(myTest$predicted == "Fog"),]$actual)-length(myTest[which((myTest$actual == "Fog") & (myTest$predicted == "Fog")),]$actual))
  results["No Event", "False Positives"] = (length(myTest[which(myTest$predicted == "No Event"),]$actual)-length(myTest[which((myTest$actual == "No Event") & (myTest$predicted == "No Event")),]$actual))
  results["Rain", "False Positives"] = (length(myTest[which(myTest$predicted == "Rain"),]$actual)-length(myTest[which((myTest$actual == "Rain") & (myTest$predicted == "Rain")),]$actual))
  results["Snow", "False Positives"] = (length(myTest[which(myTest$predicted == "Snow"),]$actual)-length(myTest[which((myTest$actual == "Snow") & (myTest$predicted == "Snow")),]$actual))
  results["Thunderstorm", "False Positives"] = (length(myTest[which(myTest$predicted == "Thunderstorm"),]$actual)-length(myTest[which((myTest$actual == "Thunderstorm") & (myTest$predicted == "Thunderstorm")),]$actual))
  results["Tornado", "False Positives"] = (length(myTest[which(myTest$predicted == "Tornado"),]$actual)-length(myTest[which((myTest$actual == "Tornado") & (myTest$predicted == "Tornado")),]$actual))
  
  #Odds of False Positive ratio
  results["Fog", "False Positives Odds"] = results["Fog", "False Positives"]/ results["Fog", "True Positives"]
  results["No Event", "False Positives Odds"] = results["No Event", "False Positives"]/ results["No Event", "True Positives"]
  results["Rain", "False Positives Odds"] = results["Rain", "False Positives"]/ results["Rain", "True Positives"]
  results["Snow", "False Positives Odds"] = results["Snow", "False Positives"]/ results["Snow", "True Positives"]
  results["Thunderstorm", "False Positives Odds"] = results["Thunderstorm", "False Positives"]/ results["Thunderstorm", "True Positives"]
  results["Tornado", "False Positives Odds"] = results["Tornado", "False Positives"]/ results["Tornado", "True Positives"]
  
  #Get Count
  results["Fog", "Count"] = (length(myTest[which(myTest$predicted == "Fog"),]$actual))
  results["No Event", "Count"] = (length(myTest[which(myTest$predicted == "No Event"),]$actual))
  results["Rain", "Count"] = (length(myTest[which(myTest$predicted == "Rain"),]$actual))
  results["Snow", "Count"] = (length(myTest[which(myTest$predicted == "Snow"),]$actual))
  results["Thunderstorm", "Count"] = (length(myTest[which(myTest$predicted == "Thunderstorm"),]$actual))
  results["Tornado", "Count"] = (length(myTest[which(myTest$predicted == "Tornado"),]$actual))
  
  results$Accuracy[is.nan(results$Accuracy)] = NA
  results$'False Positives Odds'[is.nan(results$'False Positives Odds')] = NA
  results$Accuracy[is.infinite(results$Accuracy)] = NA
  results$'False Positives Odds'[is.infinite(results$'False Positives Odds')] = NA
  results$Event = rownames(results)
  
  return(results)
  
}
plotResults <- function(testResults){
  par(mar = c(10,4,4,2) + 0.1, mfrow = c(1,2))
  barplot(height = testResults$Accuracy, 
          names.arg = rownames(testResults), 
          col = "steelblue", 
          ylab = "Model Accuracy",
          main = "Accuracy",
          las = 2,
          ylim = c(0,1))
  barplot(height = testResults$'False Positives Odds', 
          names.arg = rownames(testResults), 
          col = "steelblue", 
          ylab = "Model FP/TP",
          main = "FP/TP",
          las = 2,
          ylim=c(0,1))
}

library(plotly)

plotResults2 <- function(modelResults){
  
    plot_ly(modelResults, x = ~Accuracy, y = ~'False Positives Odds', text = ~Event, type = 'scatter', mode = 'markers',
                 marker = list(size = ~sqrt(sqrt(sqrt(nnResults$Count)))*10, opacity = 0.5)) %>%
    layout(title = 'Event-Wise Model Accuracy', 
           xaxis = list(title = 'Event-Wise Accuracy', 
                        gridcolor = 'rgb(255, 255, 255)',
                        range = c(0, 1),
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
           yaxis = list(title = 'False Positive/ True Positive Ratio',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
           paper_bgcolor = 'rgb(243, 243, 243)',
           plot_bgcolor = 'rgb(243, 243, 243)')
}




actual =    c("Fog", "Rain", "Rain", "Rain", "Tornado", "Fog", "Tornado", "Fog")
predicted = c("Fog", "Rain", "Fog", "Rain", "Tornado", "Fog", "Rain", "No Event")

testResults <-myAccuracy(actual, predicted)
