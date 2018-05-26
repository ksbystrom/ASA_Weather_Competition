library(stringr)

toClean = read.csv("final_train.csv", stringsAsFactors = FALSE)

# Check all the possible events
unique(toClean$Events)

#Looks like we need to recode the events using some regex
toClean$Fog <- str_detect(toClean$Events, "Fog")
toClean$Rain <- str_detect(toClean$Events, "Rain")
toClean$Thunderstorm <- str_detect(toClean$Events, "Thunderstorm")
toClean$Snow <- str_detect(toClean$Events, "Snow")
toClean$Tornado <- str_detect(toClean$Events, "Tornado")
toClean$Hail <- str_detect(toClean$Events, "Hail")

# Set heiarchy to detect most extreme event
# Tornado > Thunderstorm > Snow > Hail > Rain > Fog

toClean$NewEvent = ifelse(toClean$Tornado == TRUE, "Tornado", 
                          ifelse(toClean$Thunderstorm == TRUE, "Thunderstorm",
                                 ifelse(toClean$Snow == TRUE, "Snow",
                                        ifelse(toClean$Hail == TRUE, "Hail",
                                               ifelse(toClean$Rain == TRUE, "Rain", 
                                                      ifelse(toClean$Fog == TRUE, "Fog", "No Event"))))))
write.csv(toClean, "final_train_simplified.csv")

#####################################
#Repeat above code with test file

toClean = read.csv("final_test.csv", stringsAsFactors = FALSE)

# Check all the possible events
unique(toClean$Events)

#Looks like we need to recode the events using some regex
toClean$Fog <- str_detect(toClean$Events, "Fog")
toClean$Rain <- str_detect(toClean$Events, "Rain")
toClean$Thunderstorm <- str_detect(toClean$Events, "Thunderstorm")
toClean$Snow <- str_detect(toClean$Events, "Snow")
toClean$Tornado <- str_detect(toClean$Events, "Tornado")
toClean$Hail <- str_detect(toClean$Events, "Hail")

# Set heiarchy to detect most extreme event
# Tornado > Thunderstorm > Snow > Hail > Rain > Fog

toClean$NewEvent = ifelse(toClean$Tornado == TRUE, "Tornado", 
                          ifelse(toClean$Thunderstorm == TRUE, "Thunderstorm",
                                 ifelse(toClean$Snow == TRUE, "Snow",
                                        ifelse(toClean$Hail == TRUE, "Hail",
                                               ifelse(toClean$Rain == TRUE, "Rain", 
                                                      ifelse(toClean$Fog == TRUE, "Fog", "No Event"))))))
write.csv(toClean, "final_test_simplified.csv")