library(dplyr)
library(tidyverse)
library(reticulate)
library(reshape2)
library(ggplot2)

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= family_history))+
  ggtitle("Relationship between family history and seeking treatment")+
  theme_bw()+labs(fill='Family history')
  
ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= benefits))+
  ggtitle("Relationship between employer provided benefits and seeking treatment")+
  theme_bw()+labs(fill='Benefits')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= wellness_program))+
  ggtitle("Relationship between mental health being part of wellness program and seeking treatment")+
  theme_bw()+labs(fill='Wellness program')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= seek_help))+
  ggtitle("Relationship availability of resources by employer and seeking treatment")+
  theme_bw()+labs(fill='Resources available')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= mental_health_consequence))+
  ggtitle("Relationship between negative consequences due to discussion and seeking treatment")+
  theme_bw()+labs(fill='Can there be negative consequences')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= mental_health_consequence))+
  ggtitle("Relationship between possible negative consequences due to discussion and seeking treatment")+
  theme_bw()+labs(fill='Can there be negative consequences')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= supervisor))+
  ggtitle("Relationship between comfort with talking to supervisor and seeking treatment")+
  theme_bw()+labs(fill='Comfortable talking to supervisor')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= coworkers))+
  ggtitle("Relationship between comfort with talking to coworkers and seeking treatment")+
  theme_bw()+labs(fill='Comfortable talking to coworkers')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= obs_consequence))+
  ggtitle("Relationship between observed negative consequences due to discussion and seeking treatment")+
  theme_bw()+labs(fill='Have there been negative consequences')

ggplot(data = new_survey)+geom_bar(aes(x=treatment , fill= care_options))+
  ggtitle("Relationship between awareness about care options and seeking treatment")+
  theme_bw()+labs(fill='Are you aware of mental health care options your employer provides')

