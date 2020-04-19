
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
for(i in 2:nrow(survey)){
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
