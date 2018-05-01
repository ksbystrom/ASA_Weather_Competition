DataExpo2018 <- read.csv("DataExpo2018/histWeather.csv", stringsAsFactors = FALSE)

set.seed(97)
sample(1:100, size = 10)

library(gbm)
library(MASS)
library(dplyr)
library(corrplot)
library(rpart)
library(mice)

#train <- sample_n(DataExpo2018, size=10000)
train <- DataExpo2018
write.csv(train, "trainingData.csv")

#For event column, set NA values to "No Event"
train$Events <- replace(train$Events, is.na(train$Events), "No Event")
table(train$Events)

#Add a column that counts the number of NAs in the data
train$CountNA <- apply(train, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) )
table(train$CountNA)

# We probably want to remove rows with more than 8 missing values. Let's check what these rows look like
bigMissing <- train[train$CountNA > 8,] # Looks like none of these rows have any rare events so they should be safe to delete
#I will go ahead and delete these cases
train <- train[train$CountNA < 9,]

#Now I will check the values with only 1 NA because there are a lot of them
smallMissing <- train[train$CountNA == 1,] #looks like this is mostly caused by the gust variable

#Check correlation between variables
numericTrain <- train[complete.cases(train),sapply(train, is.numeric)]
m <- cor(numericTrain)
corrplot(m)

#Gustspeed seems to be correlated with mean sea level pressure and mean windspeed
gustModel <- lm(train$Max_Gust_SpeedMPH~train$Mean_Sea_Level_PressureIn+train$Mean_Wind_SpeedMPH)
#Also make model with interaction term
gustModel2 <- lm(train$Max_Gust_SpeedMPH~train$Mean_Sea_Level_PressureIn+
                   train$Mean_Wind_SpeedMPH + 
                   train$Mean_Sea_Level_PressureIn*train$Mean_Wind_SpeedMPH )
gustModel3 <- lm(train$Max_Gust_SpeedMPH~train$Mean_Sea_Level_PressureIn )
gustModel4 <- lm(train$Max_Gust_SpeedMPH~ train$Mean_Wind_SpeedMPH)
summary(gustModel)
summary(gustModel2) # Interaction term is not significant, should go with original model
summary(gustModel3)
summary(gustModel4) # This model is good because it will avoid any negative values (all coefficients are positive), is simple, and has good p-value
BIC(gustModel4)
BIC(gustModel) # model 4 has higher BIC but only by a small amount, plus has the aforementioned added benefits

plot(train$Max_Gust_SpeedMPH) #looks good, gust speeds should probably be between 0 and 100
#Impute max gust speed based on sea level and wind speed linear regression model
train$Max_Gust_SpeedMPH <- ifelse(is.na(train$Max_Gust_SpeedMPH), 
                                  coefficients(gustModel4)[1] + (coefficients(gustModel4)[2]*train$Mean_Wind_SpeedMPH), 
                                  train$Max_Gust_SpeedMPH) 
plot(train$Max_Gust_SpeedMPH) #Looks good, there are a couple of really big values, but should be ok

#Check min and max, 
summary(train$Max_Gust_SpeedMPH) # all values are positive, 

#check data set
summary(train)

#install.packages("DMwR")
library(DMwR)
train$PrecipitationIn <- as.numeric(train$PrecipitationIn)
numericTrain <- train[,sapply(train, is.numeric)]
train_complete_numeric <- knnImputation(numericTrain,k=10)
train_complete <- data.frame(train$Date, train_complete_numeric, train$Events, train$AirPtCd)
table(is.na(train_complete))