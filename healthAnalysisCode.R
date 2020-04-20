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
library(caret)
}


correctTheData<- function(data){
  #define the mistakes
  
  male <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "Male", "Male ", "Male-ish", "Male (CIS)", "Man", "Mal", "Make", "cis male", "Malr", "M", "Cis Male", "Mail", "Cis Man", "M")
  female <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail", "Female", "Female ", "Cis Female", "Femake", "Woman", "Female (cis)")
  other <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "Guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means", "Enby", "Agender", "Trans woman", "p", "All", "Guy (-ish) ^_^", "A little about you", "Nah", "Genderqueer", "Androgyne", "Female (trans)", "Neuter", "Trans-female" )
  
  #Find the mistakes and replace
  
  
  new_survey <- data
  for(i in 1:nrow(data)){
    if(data$Gender[i] %in% male == TRUE){
      new_survey$Gender[i]<- 'M'
    }
    else if(data$Gender[i] %in% female == TRUE){
      new_survey$Gender[i]<- 'F'
    }
    else if(data$Gender[i] %in% other == TRUE){
      new_survey$Gender[i]<- 'O'
    }
    #replacing the age value with median 
    if(data$Age[i]<18 | data$Age[i]>65){
      new_survey$Age[i]= median(data$Age)
    }
    
  }
  new_survey$Age <- as.numeric(new_survey$Age)
  
  #let's remove all the columns having NA's
  new_survey$Timestamp <- NULL
  new_survey$comments <- NULL
  new_survey$self_employed<-NULL
  return(new_survey)
}
cleanDataSet2014 <- function(){
  survey <- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/survey.csv",stringsAsFactors = F)
  
  #Correcting the spelling of Gender and replace the negative and values more than 65 with median of age
  new_survey <- correctTheData(survey)
  new_survey$state<-NULL
  
  #write the cleaned data to csv
  write.csv(new_survey, "C:/Users/i513930/Desktop/My/CSP571/Project/new_survey.csv", row.names = F)
  return(new_survey)
}

cleanDataSet2016 <- function(){
  # read the csv
  survey <- read.csv(
    "C:/Users/i513930/Desktop/My/CSP571/Project/survey2016.csv",
    stringsAsFactors = F)
  
  col_names <- c(
    "Age",
    "Gender",
    "Country",
    "state",
    "self_employed",
    "family_history",
    "treatment",
    "no_employees",
    "remote_work",
    "tech_company",
    "benefits",
    "care_options",
    "wellness_program",
    "seek_help",
    "anonymity",
    "leave",
    "mental_health_consequence",
    "phys_health_consequence",
    "coworkers",
    "supervisor",
    "mental_health_interview",
    "phys_health_interview",
    "mental_vs_physical",
    "obs_consequence"
  )
  
  # renaming column names
  names(survey)[names(survey) == "What.is.your.age."] <- "Age"
  names(survey)[names(survey) == "What.is.your.gender."] <- "Gender"
  names(survey)[names(survey) == "What.country.do.you.live.in."] <- "Country"
  names(survey)[names(survey) == "What.US.state.or.territory.do.you.live.in."] <- "state"
  names(survey)[names(survey) == "Are.you.self.employed."] <- "self_employed"
  names(survey)[names(survey) == "Do.you.have.a.family.history.of.mental.illness."] <- "family_history"
  names(survey)[names(survey) == "Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional."] <- "treatment"
  names(survey)[names(survey) == "If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.being.treated.effectively."] <- "work_interfere_effective"
  names(survey)[names(survey) == "If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.NOT.being.treated.effectively."] <- "work_interfere_not_effective"
  names(survey)[names(survey) == "How.many.employees.does.your.company.or.organization.have."] <- "no_employees"
  names(survey)[names(survey) == "Do.you.work.remotely."] <- "remote_work"
  names(survey)[names(survey) == "Is.your.employer.primarily.a.tech.company.organization."] <- "tech_company"
  names(survey)[names(survey) == "Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage."] <- "benefits"
  names(survey)[names(survey) == "Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage."] <- "care_options"
  names(survey)[names(survey) == "Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication.."] <- "wellness_program"
  names(survey)[names(survey) == "Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help."] <- "seek_help"
  names(survey)[names(survey) == "Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer."] <- "anonymity"
  names(survey)[names(survey) == "If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..asking.for.that.leave.would.be."] <- "leave"
  names(survey)[names(survey) == "Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences."] <- "mental_health_consequence"
  names(survey)[names(survey) == "Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences."] <- "phys_health_consequence"
  names(survey)[names(survey) == "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers."] <- "coworkers"
  names(survey)[names(survey) == "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.."] <- "supervisor"
  names(survey)[names(survey) == "Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview."] <- "mental_health_interview"
  names(survey)[names(survey) == "Would.you.be.willing.to.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview."] <- "phys_health_interview"
  names(survey)[names(survey) == "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health."] <- "mental_vs_physical"
  names(survey)[names(survey) == "Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace."] <- "obs_consequence"
  
  new_survey <- survey[, col_names]
  
  # We are assuming all employees are between the age of 18 and 75
  mean_age <- round(mean(filter(new_survey, (Age >= 18 & Age <= 65))$Age))
  
  # Replace values with mean age
  new_survey$Age[new_survey$Age < 18 | new_survey$Age > 65] <- mean_age
  
  # Formatting Gender
  new_survey$Gender <- trimws(toupper(new_survey$Gender))
  
  male <- c("M", "MALE", "MALE.", "MALE (CIS)", "MAN", "CIS MALE", "SEX IS MALE", "MALR", "DUDE", "I'M A MAN WHY DIDN'T YOU MAKE THIS A DROP DOWN QUESTION. YOU SHOULD OF ASKED SEX? AND I WOULD OF ANSWERED YES PLEASE. SERIOUSLY HOW MUCH TEXT CAN THIS TAKE?", "MAIL", "M|", "MALE/GENDERQUEER", "CISDUDE", "CIS MAN")
  female <- c("F", "FEMALE", "I IDENTIFY AS FEMALE.", "FEMALE ASSIGNED AT BIRTH", "WOMAN", "FM", "CIS FEMALE", "FEMALE OR MULTI-GENDER FEMME", "FEMALE/WOMAN", "CISGENDER FEMALE", "FEM", "CIS-WOMAN", "FEMALE (PROPS FOR MAKING THIS A FREEFORM FIELD, THOUGH)", "FEMALE-BODIED; NO FEELINGS ABOUT GENDER")
  other <- c("BIGENDER", "NON-BINARY", "TRANSITIONED, M2F", "GENDERFLUID (BORN FEMALE)", "ANDROGYNOUS", "MALE 9:1 FEMALE, ROUGHLY", "N/A", "OTHER", "NB MASCULINE", "NONE OF YOUR BUSINESS", "GENDERQUEER", "OTHER/TRANSFEMININE", "HUMAN", "GENDERFLUID", "ENBY", "GENDERQUEER WOMAN", "MTF", "QUEER", "AGENDER", "FLUID", "NONBINARY", "UNICORN", "MALE (TRANS, FTM)", "GENDERFLUX DEMI-GIRL", "", "AFAB", "TRANSGENDER WOMAN")
  
  for(i in 1:nrow(survey)){
    if(new_survey$Gender[i] %in% male){
      new_survey$Gender[i]<- 'M'
    }
    if(new_survey$Gender[i] %in% female == TRUE){
      new_survey$Gender[i]<- 'F'
    }
    if(new_survey$Gender[i] %in% other == TRUE){
      new_survey$Gender[i]<- 'O'
    }
  }
  
  # Check if we missed any genders
  new_survey[with(new_survey, grepl("[^FMO]$", Gender)),"Gender"]
  
  col_names <- c("work_interfere_effective", "work_interfere_not_effective")
  
  # Combine the interfere levels
  interfere_levels = c("Not applicable to me", "Never", "Rarely", "Sometimes", "Often")
  a <- as.numeric(ordered(survey$work_interfere_effective, interfere_levels))
  b <- as.numeric(ordered(survey$work_interfere_not_effective, interfere_levels))
  
  for(i in 1:nrow(survey)) {
    new_survey$work_interfere[i] <- ceiling(mean(c(a[i], b[i])))
  }
  
  new_survey$work_interfere <- sapply(new_survey$work_interfere, function(x) 
    switch(x, 
           "Not applicable to me", 
           "Never", 
           "Rarely", 
           "Sometimes", 
           "Often"
    )
  )
  
  new_survey$tech_company <- sapply(new_survey$tech_company, function(x) 
    if(is.na(x))
      return(x)
    else if(x == 0)
      return("No")
    else if(x == 1)
      return("Yes")
  )
  
  write.csv(new_survey, "C:/Users/i513930/Desktop/My/CSP571/Project/new_survey2016.csv", row.names = FALSE)
  return(new_survey)
}

dataVisualize_geomBar<- function(data,aesValue,fillValue,angleValue,vjustValue,titleValue){
    g2 <- ggplot(data, aes(aesValue))
    g2 + geom_bar(aes(fill=fillValue),width = 0.5) + 
    theme(axis.text.x = element_text(angle=angleValue, vjust=vjustValue)) + 
    labs(title=titleValue)+
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

dataVisualize_geomBar2016 <- function(data,aesValue,fillValue,angleValue,vjustValue,titleValue){
  g2 <- ggplot(data, aes(aesValue))
  g2 + geom_bar(aes(fill=fillValue),width = 0.5) + 
    theme(axis.text.x = element_text(angle=angleValue, vjust=vjustValue)) + 
    labs(title=titleValue)+
    scale_fill_manual(
      labels = c("Empty Values", "Maybe", "No", "Yes"), 
      values=c("#5b137d", "#c94426", "#3780c4", "#68a819"))+
    ylab("")+
    labs(fill='Legend')
}

dataVisualize_geomBar2016_1 <- function(data,aesValue,fillValue,angleValue,vjustValue,titleValue){
  g2 <- ggplot(data, aes(aesValue))
  g2 + geom_bar(aes(fill=fillValue),width = 0.5) + 
    theme(axis.text.x = element_text(angle=angleValue, vjust=vjustValue)) + 
    labs(title=titleValue)+
    scale_fill_manual(
      labels = c("Empty Values", "Maybe", "No", "Not Eligible", "Yes"),
      values=c("#5b137d", "#3780c4", "#c94426", "#faaa2a","#68a819"))+
    ylab("")+
    labs(fill='Legend')+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}
whichPartOfWorldIsMostAffected<- function(new_survey){
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
  
  data$work_interfere <- factor(data$work_interfere, levels = c("NA","Never", "Rarely", "Sometimes", "Often"))
  data$benefits <- factor(data$benefits, levels = c("No", "Don't know", "Yes"))
  data$wellness_program <- factor(data$wellness_program, levels = c("No", "Don't know", "Yes"))
  data$anonymity <- factor(data$anonymity, levels = c("No", "Don't know", "Yes"))
  data$leave <- factor(data$leave, levels = c("Don't know", "Very difficult", "Somewhat difficult" , "Somewhat easy" , "Very easy"))
  data$phys_health_consequence <- factor(data$phys_health_consequence, levels = c("No", "Maybe", "Yes"))
  data$mental_health_interview <- factor(data$mental_health_interview, levels = c("No", "Maybe", "Yes"))
  data$mental_vs_physical <- factor(data$mental_vs_physical, levels = c("No", "Don't know", "Yes"))
  data$seek_help <- factor(data$seek_help, levels = c("No", "Don't know", "Yes"))
  data$Region <- factor(data$Region, levels=c("US","Europe","NorthAmerica_other","Oceana","Asia","South_Central_America","Africa"))
  #data$state <- factor(data$state, levels=c("NA","AL","AK","AZ","AR","CA","CO","CT","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
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
  summary(survey_rf)
  return(survey_rf)
}

predictmethod<- function(data,newdataValue,typeValue){
  return(predict(data, newdata = newdataValue,na.action = na.roughfix,type= typeValue))
}

confusionMatrix<- function(predictedValue, actualValue){
  return(table(Predicted = predictedValue, Actual = actualValue))
}

calError<- function(data){
  return(1 - sum(diag(data))/sum(data))
}


logReg<- function(dataSet){
  return(glm(treatment ~ ., data=dataSet,na.action = na.exclude,family="binomial"))
}
calPerformance<- function(predValue){
  return(performance(predValue, measure="tpr", x.measure="fpr"))
}

predictmethodforlg<- function(data,newdataValue,typeValue){
  return(predict(data, newdata = newdataValue,na.action = na.exclude,type= typeValue))
}

#function calls for 2014 dataset
loadlibraries()
new_survey = cleanDataSet2014()

#Analysis
dataVisualize_geomBar(new_survey,new_survey$Age,new_survey$supervisor,45,0.5,"Comfortable discussing issues with Supervisors according to age")
dataVisualize_geomBar(new_survey,new_survey$Age,new_survey$coworkers,65,0.6,"Comfortable discussing issues with coworkers according to age")
dataVisualize_geomBar(new_survey,new_survey$benefits,new_survey$benefits,45,0.5,"Does the employer provide mental health benefits")
dataVisualize_barPlot(new_survey)
dataVisualize_barPlot1(new_survey)
new_survey = whichPartOfWorldIsMostAffected(new_survey)
#USA is most affected.


new_survey= reorderingData(new_survey)
listdata = divideData(new_survey)
print(paste("Data is divided as Train and Test :",nrow(listdata$train),",",nrow(listdata$test)))

#model
#1. randomForest method
rf_value = randomForestMethod(listdata$train)

rf_predictValue = predictmethod(rf_value,listdata$test,"class")
rf_pred1 = predictmethod(rf_value,listdata$test,"response")
rf_pValue = prediction(as.numeric(rf_pred1),as.numeric(listdata$test$treatment))
rf_performance = calPerformance(rf_pValue)

rf_cf = confusionMatrix(rf_predictValue, listdata$test$treatment)
rf_error = calError(rf_cf)

print(paste("Error for RandomForest :",rf_error))

varImpPlot(rf_value, sort=T)

tableforRf <- table(rf_predictValue,listdata$test$treatment )
recallValue_rf =recall(tableforRf)
precisionValue_rf = precision(tableforRf)
f_rf = F_meas(tableforRf)

print(paste("Precision and recall and F1 score values for Random forest: ",precisionValue_rf,",",recallValue_rf,",",f_rf))

#2. Logistic Regression

lg_value = logReg(listdata$train)
lg_predictValue = predictmethodforlg(lg_value, listdata$test,"response")

lg_pValue = prediction(as.numeric(lg_predictValue),as.numeric(listdata$test$treatment))
lg_performance = calPerformance(lg_pValue)

# Save the predicted labels using 0.5 as a threshold
lg_test <-  listdata$test %>%
  mutate(predTreatment=as.factor(ifelse(lg_predictValue<=0.5, "No", "Yes")))

lg_cf = confusionMatrix(lg_test$predTreatment,lg_test$treatment)

lg_error = calError(lg_cf)


print(paste("Error for Logisticregression :",lg_error))


tableforlg <- table(lg_test$predTreatment,listdata$test$treatment )
recallValue_lg =recall(tableforlg)
precisionValue_lg = precision(tableforlg)
f_lg = F_meas(tableforlg)

print(paste("Precision and recall and F1 score values for Random forest: ",precisionValue_lg,",",recallValue_lg,",",f_lg))



#comparsion plot
plot(rf_performance, main="ROC Curve", col = "#3780c4")
plot(lg_performance, add = TRUE, col = "#c94426")
axis(side=1,at=seq(0,1,.1))
axis(side=2,at=seq(0,1,.1))
legend(0.5, 0.2, legend = c("Random Forest", "Logistic Regression"), col = c("#3780c4", "#c94426"), lty = 1:1, cex = 0.5)


#with ranking Logistic Regression 
survey <- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/survey.csv",stringsAsFactors = F)
final_rank<-correctTheData(survey)
rank_values<- read.csv("C:/Users/i513930/Desktop/My/CSP571/Project/rankvalues.csv",stringsAsFactors = TRUE)

final_rank$rank<- rank_values$ï..rank
final_rank$rank <- factor(final_rank$rank, levels = c("NA","1", "6", "7", "8","10","11","12","13","15","16","17","18","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","38","39","40","41","42","43","44","45","46","47","48","49","50"))
final_rank<- whichPartOfWorldIsMostAffected(final_rank)
final_rank$state <- factor(final_rank$state, levels=c("NA","AL","AZ","AR","CA","CO","CT","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
final_rank= reorderingData(final_rank)
listdata = divideData(final_rank)
print(paste("Data is divided as Train and Test :",nrow(listdata$train),",",nrow(listdata$test)))



rf_value = randomForestMethod(listdata$train)

rf_predictValue = predictmethod(rf_value,listdata$test,"class")
rf_pValue = prediction(as.numeric(rf_pred1),as.numeric(listdata$test$treatment))


rf_cf = confusionMatrix(rf_predictValue, listdata$test$treatment)
rf_error = calError(rf_cf)

print(paste("Error for RandomForest with Rank :",rf_error))

varImpPlot(rf_value, sort=T)

tableforRf <- table(rf_predictValue,listdata$test$treatment )
recallValue_rf =recall(tableforRf)
precisionValue_rf = precision(tableforRf)
f_rf = F_meas(tableforRf)

print(paste("Precision and recall and F1 score values for Random forest with Rank: ",precisionValue_rf,",",recallValue_rf,",",f_rf))







#2016 dataset

new_survey2016 = cleanDataSet2016()
levels(factor(new_survey2016$coworkers))
#Analysis
dataVisualize_geomBar2016(new_survey2016,new_survey2016$Age,new_survey2016$supervisor,45,0.5,"Comfortable discussing issues with Supervisors according to age")
dataVisualize_geomBar2016(new_survey2016,new_survey2016$Age,new_survey2016$coworkers,65,0.6,"Comfortable discussing issues with coworkers according to age")
dataVisualize_geomBar2016_1(new_survey2016,new_survey2016$benefits,new_survey2016$benefits,45,0.5,"Does the employer provide mental health benefits")
dataVisualize_barPlot(new_survey2016)
dataVisualize_barPlot1(new_survey2016)
new_survey2016 = whichPartOfWorldIsMostAffected(new_survey2016)
#USA is most affected.


new_survey2016= reorderingData(new_survey2016)
listdata_2016 = divideData(new_survey2016)
print(paste("Data is divided as Train and Test :",nrow(listdata_2016$train),",",nrow(listdata_2016$test)))

# Random Forest


rf_value_2016 = randomForestMethod(listdata_2016$train)

rf_predictValue_2016 = predictmethod(rf_value_2016,listdata_2016$test,"class")

rf_cf_2016 = confusionMatrix(rf_predictValue_2016, listdata_2016$test$treatment)
rf_error_2016 = calError(rf_cf_2016)

print(paste("Error for RandomForest for 2016 dataset :",rf_error_2016))
