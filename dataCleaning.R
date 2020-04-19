
#read the csv
library(dplyr)
library(tidyverse)

library(reticulate)

library(reshape2)
library(ggplot2)
#specifying which version of python to use


survey <- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/survey.csv",stringsAsFactors = F)


#1 Correcting the spelling of Gender and replace the negative and values more than 65 with median of age

#define the mistakes

male <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "Male", "Male ", "Male-ish", "Male (CIS)", "Man", "Mal", "Make", "cis male", "Malr", "M", "Cis Male", "Mail", "Cis Man", "M")
female <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail", "Female", "Female ", "Cis Female", "Femake", "Woman", "Female (cis)")
other <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "Guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means", "Enby", "Agender", "Trans woman", "p", "All", "Guy (-ish) ^_^", "A little about you", "Nah", "Genderqueer", "Androgyne", "Female (trans)", "Neuter", "Trans-female" )

#Find the mistakes and replace

new_survey <- survey
for(i in 1:nrow(survey)){
  if(survey$Gender[i] %in% male == TRUE | survey$Gender[i]=="Male"){
    new_survey$Gender[i]<- 'M'
  }
  else if(survey$Gender[i] %in% female == TRUE | survey$Gender[i]=="Female"){
    new_survey$Gender[i]<- 'F'
  }
  else if(survey$Gender[i] %in% other == TRUE){
    new_survey$Gender[i]<- 'O'
  }
  if(survey$Age[i]<18 | survey$Age[i]>65){
    new_survey$Age[i]= median(survey$Age)
  }
  
}
new_survey$Age <- as.numeric(new_survey$Age)
new_survey$Timestamp <- NULL
new_survey$comments <- NULL

write.csv(new_survey, "C:/Users/i513930/Desktop/My/CSP571/Project/new_survey.csv", row.names = F)

#How does the size of a company relate to an employer formally discussing mental health

counts <- table(new_survey$wellness_program, new_survey$no_employees)
barplot(counts, main="Size of company and Employer providing health",
        xlab="Number of Employees", ylab ="Employer providing health",col=c("darkblue","red","green")
        ,beside=TRUE,legend.text = c("Yes", "No","NA"))

#How does the age of an employee relate to their comfort in discussing mental health issues with their peers
counts <- table(new_survey$supervisor, new_survey$Age)
barplot(counts, main="Size of company and Employer providing health",
        xlab="Age", ylab ="Willing to discuss with coworkers",col=c("darkblue","red","green")
        ,beside=FALSE,
        legend.text = c("Yes", "No","NA"),args.legend = list(x = "topright"))



#which part of the world are most affected.

colnames(new_survey)[colnames(new_survey)=="Country"] <- "Region"

grouped_countries <- fct_collapse(new_survey$Region, 
                                  US = c("United States"),
                                  NorthAmerica_other = c("Canada", "Mexico", "Bahamas, The"),
                                  Asia = c("India","Singapore","Japan","China","Philippines","Thailand","Israel"),
                                  Europe = c("United Kingdom","Bulgaria","France","Portugal","Slovenia","Italy",          
                                             "Sweden","Austria","Ireland","Germany","Russia","Netherlands",
                                             "Switzerland","Poland","Latvia","Romania","Spain","Finland",                
                                             "Belgium","Bosnia and Herzegovina","Croatia", "Norway", "Greece",                
                                             "Moldova","Georgia",'Czech Republic', "Hungary","Denmark"),
                                  Africa = c("South Africa", "Zimbabwe", "Nigeria"),
                                  Oceana = c("Australia", "New Zealand"),
                                  South_Central_America = c("Costa Rica", "Brazil", "Colombia", "Uruguay")
                                  )

fct_count(grouped_countries,sort = TRUE)

#US is most affected then other Regions


#Does the employer provide mental health benefits
counts <- table(new_survey$benefits)
barplot(counts, main="Employer providing benfits",
        xlab="Health benefits", ylab ="Employer",col=c("darkblue","red","green")
        ,beside=TRUE)

#What Type of companies has more mental health issues
counts <- table(new_survey$tech_company)
barplot(counts, main="",
        xlab="Tech company or not", ylab ="Count",col=c("darkblue","red","green")
        ,beside=TRUE)


#Are the employees comfortable talking about mental health with their coworkers.

counts <- table(new_survey$coworkers, new_survey$Age)
barplot(counts, main="Size of company and Employer providing health",
        xlab="Age", ylab ="Willing to discuss with coworkers",col=c("darkblue","red","green")
        ,beside=FALSE,
        legend.text = c("Yes", "No","NA"),args.legend = list(x = "topright"))
  
# Reordering dataset to make predicting column to be last
# Converting char columns to factors

new_survey <- new_survey[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,15)]    
new_survey <- mutate_if(new_survey, is.character, as.factor)


# Handling the factor levels 

new_survey$work_interfere <- factor(new_survey$work_interfere, levels = c("Never", "Rarely", "Sometimes", "Often"))
new_survey$benefits <- factor(new_survey$benefits, levels = c("No", "Don't know", "Yes"))
new_survey$wellness_program <- factor(new_survey$wellness_program, levels = c("No", "Don't know", "Yes"))
new_survey$anonymity <- factor(new_survey$anonymity, levels = c("No", "Don't know", "Yes"))
new_survey$leave <- factor(new_survey$leave, levels = c("Don't know", "Very difficult", "Somewhat difficult" , "Somewhat easy" , "Very easy"))
new_survey$phys_health_consequence <- factor(new_survey$phys_health_consequence, levels = c("No", "Maybe", "Yes"))
new_survey$mental_health_interview <- factor(new_survey$mental_health_interview, levels = c("No", "Maybe", "Yes"))
new_survey$mental_vs_physical <- factor(new_survey$mental_vs_physical, levels = c("No", "Don't know", "Yes"))
new_survey$seek_help <- factor(new_survey$seek_help, levels = c("No", "Don't know", "Yes"))

#  Splitting data into test and train,

set.seed(101) 
sample <- sample.int(n = nrow(new_survey), size = floor(.80*nrow(new_survey)), replace = F)
train <- new_survey[sample, ]
test  <- new_survey[-sample, ]

# Feature selection
library('randomForest')

# Using random forest for variable selection
rfModel <- randomForest(seek_help ~ ., data = train, na.action = na.roughfix)

# Getting the list of important variables
importance(rfModel)
# We will drop variables with mean decrease in Gini < 10
