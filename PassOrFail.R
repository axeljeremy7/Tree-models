#!/usr/local/bin/Rscript

#Binary Classification (Pass or Fail) Analysis regarding Math grades

#The aim of this report is to have a possibly high interpretability level of a model with
#respect of the student math performance
#The interpretability level means that the plots have enough leaf nodes which helps to
#explain the variables


# The variable G3 helps to determine the binary level of pass and fail, 
#In addition, I am going to do three extras configurations:

#Configurations:
# A. With all variables except G3
# B. With all the variables except G3 and G2. 
# C. With all the variables except G3 and G1. 
# D. With all the variables except G3, G2 and G1. 

#Setting
library(tree)
student = read.csv("mat.csv",header = T,sep = ";")
firstSetting = function(){
  binClass=ifelse(student$G3>=10,"Pass","Fail")
  student=data.frame(student,binClass)
  student$MotherEducation = as.factor(student$MotherEducation)
  student$FatherEducation = as.factor(student$FatherEducation)
  student$traveltime = as.factor(student$traveltime)
  student$studytime = as.factor(student$studytime)
  student$famrel = as.factor(student$famrel)
  student$freetime = as.factor(student$freetime)
  student$goout = as.factor(student$goout)
  student$Dalc = as.factor(student$Dalc)
  student$Walc = as.factor(student$Walc)
  student$health = as.factor(student$health)
  student = student[,-33]#Eliminate the G3 variable because it's no longer needed
  return(student)
}


#View(student)#works only in RStudio
student = firstSetting()
#Decision Trees
source("ConfigA.R",echo=T)
source("ConfigB.R",echo=T)
source("ConfigC.R",echo=T)
source("ConfigD.R",echo=T)
#Random Forest
source("RandomForestCheck.R")

