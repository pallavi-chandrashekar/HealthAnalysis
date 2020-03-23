library(tidyverse)
library(ggplot2)
library(tree)
library(plyr)
library(randomForest)
library(class)
library(rpart)
library(maptree)
library(ROCR)
library(reshape2)
library(dplyr)
library(glmnet)
library(forcats)
library(knncat)

#read the csv

survey <- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/survey.csv", stringsAsFactors = F)


#1 Correcting the spelling of Gender

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
}
for(i in 1:nrow(survey)){
  if(survey$Gender[i] %in% female == TRUE | survey$Gender[i]=="Female"){
    new_survey$Gender[i]<- 'F'
  }
}
for(i in 1:nrow(survey)){
  if(survey$Gender[i] %in% other == TRUE){
    new_survey$Gender[i]<- 'O'
  }
}  

#2 Correcting the no_employees

jun<- c("25-Jun","6/25/2020")
jan<- c("5-Jan","1/5/2020")

for(i in 2:nrow(survey)){
  if(survey$no_employees[i] %in% jun == TRUE){
    new_survey$no_employees[i]<- '6-25'
  }
}
for(i in 2:nrow(survey)){
  if(survey$no_employees[i] %in% jan == TRUE){
    new_survey$no_employees[i]<- '1-5'
  }
}

write.csv(new_survey, "C:/Users/i513930/Desktop/My/CSP571/Project/new_survey.csv")

