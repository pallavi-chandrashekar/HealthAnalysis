library(tidyverse)
library(stringr)
# Loading the dataset, skipping the unnecessary rows


dataset <- read.csv("C:/Users/Priyanka/Desktop/DPA/Project/secondary_dataset.csv", skip = 4, header = TRUE)
                  

summary(dataset)

#Removing unnecessary rows
dataset <- dataset[-c(2580,2581,2582,2583,2584,2585,2586,2587),]

sapply(dataset, class)

#some columns are factors which need to be converted to numeric

cols= length(colnames(dataset))

for (i in 3:cols){
  if ((class(dataset[[i]]))== 'factor')
    dataset[[i]] <- as.numeric(as.character(dataset[[i]]))
}

summary(dataset)

# Assigning state names

k <- dataset[1,1]

for (i in 2: nrow(dataset))
{
  if (dataset[i,1] == "")
    dataset[i,1] <- k
  else 
    k <- dataset[i,1]
}

dataset$State <- trimws(tolower(dataset$State))

dataset$State <- sub("alabama - [a-z0-9]* [a-z0-9]*", "AL", dataset$State)
dataset$State <- sub("alaska - [a-z0-9]* [a-z0-9]*", "AK", dataset$State)
dataset$State <- sub("arizona - [a-z0-9]* [a-z0-9]*", "AZ", dataset$State)
dataset$State <- sub("arkansas - [a-z0-9]* [a-z0-9]*", "AR", dataset$State)
dataset$State <- sub("california - [a-z0-9]* [a-z0-9]*", "CA", dataset$State)
dataset$State <- sub("colorado - [a-z0-9]* [a-z0-9]*", "CO", dataset$State)
dataset$State <- sub("connecticut - [a-z0-9]* [a-z0-9]*", "CT", dataset$State)
dataset$State <- sub("delaware - [a-z0-9]* [a-z0-9]*", "DE", dataset$State)
dataset$State <- sub("florida - [a-z0-9]* [a-z0-9]*", "FL", dataset$State)
dataset$State <- sub("georgia - [a-z0-9]* [a-z0-9]*", "GA", dataset$State)
dataset$State <- sub("hawaii - [a-z0-9]* [a-z0-9]*", "HI", dataset$State)
dataset$State <- sub("idaho - [a-z0-9]* [a-z0-9]*", "ID", dataset$State)
dataset$State <- sub("illinois - [a-z0-9]* [a-z0-9]*", "IL", dataset$State)
dataset$State <- sub("indiana - [a-z0-9]* [a-z0-9]*", "IN", dataset$State)
dataset$State <- sub("iowa - [a-z0-9]* [a-z0-9]*", "IA", dataset$State)
dataset$State <- sub("kansas - [a-z0-9]* [a-z0-9]*", "KS", dataset$State)
dataset$State <- sub("kentucky - [a-z0-9]* [a-z0-9]*", "KY", dataset$State)
dataset$State <- sub("louisiana - [a-z0-9]* [a-z0-9]*", "LA", dataset$State)
dataset$State <- sub("maine - [a-z0-9]* [a-z0-9]*", "ME", dataset$State)
dataset$State <- sub("maryland - [a-z0-9]* [a-z0-9]*", "MD", dataset$State)
dataset$State <- sub("massachussetts - [a-z0-9]* [a-z0-9]*", "MA", dataset$State)
dataset$State <- sub("michigan - [a-z0-9]* [a-z0-9]*", "MI", dataset$State)
dataset$State <- sub("minnesota - [a-z0-9]* [a-z0-9]*", "MN", dataset$State)
dataset$State <- sub("mississippi - [a-z0-9]* [a-z0-9]*", "MS", dataset$State)
dataset$State <- sub("missouri - [a-z0-9]* [a-z0-9]*", "MO", dataset$State)
dataset$State <- sub("montana - [a-z0-9]* [a-z0-9]*", "MT", dataset$State)
dataset$State <- sub("nebraska - [a-z0-9]* [a-z0-9]*", "NE", dataset$State)
dataset$State <- sub("nevada - [a-z0-9]* [a-z0-9]*", "NV", dataset$State)
dataset$State <- sub("new hampshire - [a-z0-9]* [a-z0-9]*", "NH", dataset$State)
dataset$State <- sub("new jersey - [a-z0-9]* [a-z0-9]*", "NJ", dataset$State)
dataset$State <- sub("new mexico - [a-z0-9]* [a-z0-9]*", "NM", dataset$State)
dataset$State <- sub("new york - [a-z0-9]* [a-z0-9]*", "NY", dataset$State)
dataset$State <- sub("north carolina - [a-z0-9]* [a-z0-9]*", "NC", dataset$State)
dataset$State <- sub("north dakota - [a-z0-9]* [a-z0-9]*", "ND", dataset$State)
dataset$State <- sub("ohio - [a-z0-9]* [a-z0-9]*", "OH", dataset$State)
dataset$State <- sub("oklahoma - [a-z0-9]* [a-z0-9]*", "OK", dataset$State)
dataset$State <- sub("oregon - [a-z0-9]* [a-z0-9]*", "OR", dataset$State)
dataset$State <- sub("pennsylvania - [a-z0-9]* [a-z0-9]*", "PA", dataset$State)
dataset$State <- sub("rhode island - [a-z0-9]* [a-z0-9]*", "RI", dataset$State)
dataset$State <- sub("south carolina - [a-z0-9]* [a-z0-9]*", "SC", dataset$State)
dataset$State <- sub("south dakota - [a-z0-9]* [a-z0-9]*", "SD", dataset$State)
dataset$State <- sub("tennessee - [a-z0-9]* [a-z0-9]*", "TN", dataset$State)
dataset$State <- sub("texas - [a-z0-9]* [a-z0-9]*", "TX", dataset$State)
dataset$State <- sub("utah - [a-z0-9]* [a-z0-9]*", "UT", dataset$State)
dataset$State <- sub("vermont - [a-z0-9]* [a-z0-9]*", "VT", dataset$State)
dataset$State <- sub("virginia - [a-z0-9]* [a-z0-9]*", "VA", dataset$State)
dataset$State <- sub("washington - [a-z0-9]* [a-z0-9]*", "WA", dataset$State)
dataset$State <- sub("west virginia - [a-z0-9]* [a-z0-9]*", "WV", dataset$State)
dataset$State <- sub("wisconsin - [a-z0-9]* [a-z0-9]*", "WI", dataset$State)
dataset$State <- sub("wyoming - [a-z0-9]* [a-z0-9]*", "WY", dataset$State)




#Spell checking the counties
#The names of the counties are irrelevant because we are only concerned with the total number of crimes in each state
#So we ignore the spelling mistakes in the county column


#Handling NA's
#Since, all the crime values are in integers, they can be replaced with 0's

dataset[is.na(dataset)] <- 0

#Totalling crimes in each county

dataset$total <- rowSums(dataset[3:13])


#Creating new dataframe with state and number of crimes
library(plyr)
finalrank <- data.frame(state.abb)
finalrank$total_crime <- 0



for( j in 1: nrow(finalrank)){
  for (i in 1: nrow(dataset)){
    if (dataset[i,1] == finalrank[j,1]){
      finalrank[j,2] <- finalrank[j,2] + dataset[i,14]
    }
  }
}


#Sorting and ranking
finalrank$rank <- rank(finalrank$total_crime, ties.method = "min")







