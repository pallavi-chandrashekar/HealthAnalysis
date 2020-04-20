#define the functions

#libraries required
loadlibraries <-function(){
library(dplyr)
library(tidyverse)
library(reticulate)
library(reshape2)
library(ggplot2)
library(tree)
library(randomForest)
}

cleanDataSet2014 <- function(){
  survey <- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/survey.csv",stringsAsFactors = F)
  
  
  #Correcting the spelling of Gender and replace the negative and values more than 65 with median of age
  
  #define the mistakes
  
  male <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "Male", "Male ", "Male-ish", "Male (CIS)", "Man", "Mal", "Make", "cis male", "Malr", "M", "Cis Male", "Mail", "Cis Man", "M")
  female <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail", "Female", "Female ", "Cis Female", "Femake", "Woman", "Female (cis)")
  other <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "Guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means", "Enby", "Agender", "Trans woman", "p", "All", "Guy (-ish) ^_^", "A little about you", "Nah", "Genderqueer", "Androgyne", "Female (trans)", "Neuter", "Trans-female" )
  
  #Find the mistakes and replace
  
  new_survey <- survey
  for(i in 1:nrow(survey)){
    if(survey$Gender[i] %in% male == TRUE){
      new_survey$Gender[i]<- 'M'
    }
    else if(survey$Gender[i] %in% female == TRUE){
      new_survey$Gender[i]<- 'F'
    }
    else if(survey$Gender[i] %in% other == TRUE){
      new_survey$Gender[i]<- 'O'
    }
    #replacing the age value with median 
    if(survey$Age[i]<18 | survey$Age[i]>65){
      new_survey$Age[i]= median(survey$Age)
    }
  }
  new_survey$Age <- as.numeric(new_survey$Age)
  
  #let's remove all the columns having NA's
  new_survey$Timestamp <- NULL
  new_survey$comments <- NULL
  new_survey$self_employed<-NULL
  new_survey$state<-NULL
  
  #write the cleaned data to csv
  write.csv(new_survey, "C:/Users/i513930/Desktop/My/CSP571/Project/new_survey.csv", row.names = F)
  return(new_survey)
}


dataVisualize_geomBar<- function(data,aesValue,fillValue,angleValue,vjustValue,titleValue){
    g2 <- ggplot(data, aes(aesValue))
    g2 + geom_bar(aes(fill=fillValue),width = 0.5) + 
    theme(axis.text.x = element_text(angle=angleValue, vjust=vjustValue)) + 
    labs(title=titleValue)+
    facet_grid(~ fillValue)+
    scale_fill_manual(values=c("#3780c4","#c94426","#68a819"))+
    ylab("")+
    labs(fill='Legend')
}

dataVisualize_barPlot<- function(data){
  counts <- table(data$wellness_program, data$no_employees)
  barplot(counts, main="Does the employer discuss mental health issues, based on the size of the company",
          xlab="Number of Employees", ylab ="Employer discusses mental health",col=c("#3780c4","#c94426","#68a819")
          ,beside=TRUE,legend.text = c("Yes", "No","NA"))
}
dataVisualize_barPlot1<-function(data){
  counts <- table(data$tech_company)
  barplot(counts, main="What Type of companies has more mental health issues",
          xlab="Tech company or not", ylab ="Count",col=c("#3780c4","#c94426","#68a819")
          ,beside=TRUE)
}
whichPartOfWorldIsMostAffected<- function(){
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
  
  print(fct_count(grouped_countries,sort = TRUE))
  new_survey$Region <- grouped_countries
  return(new_survey)
}


reorderingData<- function(data){
  # Converting char columns to factors
  
  #data <- data[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,15)]    
  data <- mutate_if(data, is.character, as.factor)
  
  
  # Handling the factor levels 
  
  data$work_interfere <- factor(data$work_interfere, levels = c("Never", "Rarely", "Sometimes", "Often"))
  data$benefits <- factor(data$benefits, levels = c("No", "Don't know", "Yes"))
  data$wellness_program <- factor(data$wellness_program, levels = c("No", "Don't know", "Yes"))
  data$anonymity <- factor(data$anonymity, levels = c("No", "Don't know", "Yes"))
  data$leave <- factor(data$leave, levels = c("Don't know", "Very difficult", "Somewhat difficult" , "Somewhat easy" , "Very easy"))
  data$phys_health_consequence <- factor(data$phys_health_consequence, levels = c("No", "Maybe", "Yes"))
  data$mental_health_interview <- factor(data$mental_health_interview, levels = c("No", "Maybe", "Yes"))
  data$mental_vs_physical <- factor(data$mental_vs_physical, levels = c("No", "Don't know", "Yes"))
  data$seek_help <- factor(data$seek_help, levels = c("No", "Don't know", "Yes"))
  data$Region <- factor(data$Region, levels=c("US","Europe","NorthAmerica_other","Oceana","Asia","South_Central_America","Africa"))
  return(data)
}


divideData<- function(data){
  set.seed(101) 
  sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]
  result<- list("train" = train,"test" = test)
  return(result)
}

randomForestMethod<- function(dataSet){
  set.seed(1)
  survey_rf <- randomForest(treatment~., 
                            data=dataSet,
                            importance=T, 
                            na.action = na.roughfix)
  return(survey_rf)

}

predictmethod<- function(data,newdataValue,typeValue){
  return(predict(data, newdata = newdataValue, na.action = na.roughfix,type= typeValue))
  
}

confusionMatrix<- function(predictedValue, actualValue){
  return(table(Predicted = predictedValue, Actual = actualValue))
}

calError<- function(data){
  return(1 - sum(diag(data))/sum(data))
}


logReg<- function(dataSet){
  return(glm(treatment ~ ., data=dataSet, na.action = na.roughfix,family="binomial"))
}

#function calls
loadlibraries()
new_survey = cleanDataSet2014()

#Analysis
dataVisualize_geomBar(new_survey,new_survey$Age,new_survey$supervisor,45,0.5,"Comfortable discussing issues with Supervisors according to age")
dataVisualize_geomBar(new_survey,new_survey$Age,new_survey$coworkers,65,0.6,"Comfortable discussing issues with coworkers according to age")
dataVisualize_geomBar(new_survey,new_survey$benefits,new_survey$benefits,45,0.5,"Does the employer provide mental health benefits")
dataVisualize_barPlot(new_survey)
dataVisualize_barPlot1(new_survey)
new_survey = whichPartOfWorldIsMostAffected()
#USA is most affected.


new_survey= reorderingData(new_survey)
listdata = divideData(new_survey)
print(paste("Data is divided as Train and Test :",nrow(listdata$train),",",nrow(listdata$test)))

#model
#1. randomForest method
rf_value = randomForestMethod(listdata$train)

rf_predictValue = predictmethod(rf_value,listdata$test,"class")
rf_cf = confusionMatrix(rf_predictValue, listdata$test$treatment)
rf_error = calError(rf_cf)

print(paste("Error for RandomForest :",rf_error))

varImpPlot(rf_value, sort=T)


#2. Logistic Regression

lg_value = logReg(listdata$train)
lg_predictValue = predictmethod(lg_value, listdata$test,"response")

# Save the predicted labels using 0.5 as a threshold
lg_test <-  listdata$test %>%
  mutate(predTreatment=as.factor(ifelse(lg_predictValue<=0.5, "No", "Yes")))

lg_cf = confusionMatrix(lg_test$predTreatment,lg_test$treatment)

lg_error = calError(lg_cf)

print(paste("Error for Logisticregression :",lg_error))


