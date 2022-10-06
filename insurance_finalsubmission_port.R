rm(list=ls())
library(gmodels)
library(skimr)
library(moments)
library(modelr)
library(tidyverse)
library(moderndive)
insurance<-read.csv("insurance_exam.csv")
summary(insurance)
skim(insurance)
colSums(is.na(insurance))
set.seed(17)


insurance<-mutate(insurance, highBMI = ifelse(insurance$bmi>30,1,0))
insurance<-mutate(insurance, highBMIsmoke = ifelse(insurance$bmi>30 & insurance$smoker== "yes",1,0))
insurance<-mutate(insurance, malesmoker = ifelse(insurance$sex == "male" & insurance$smoker == "yes", 1,0))
insurance<-mutate(insurance, agesmoker = ifelse(insurance$age > 45 & insurance$smoker == "yes", 1,0))
insurance<-mutate(insurance, parentsmoke = ifelse(insurance$children >= 1 & insurance$smoker == "yes", 1,0))
insurance <- mutate(insurance, southeastsmoke  = ifelse(insurance$region == "southeast" & insurance$smoker == "yes",1,0))
insurance <- mutate(insurance, southwestsmoke  = ifelse(insurance$region == "southwest" & insurance$smoker == "yes",1,0))
insurance <- mutate(insurance, highBMIsmokeage = ifelse(insurance$bmi >30 & insurance$age > 45 & insurance$smoker == "yes", 1,0))



insurance_test <- sample_frac(insurance, 0.20)
insurance_train <- insurance %>% filter(!(index %in% insurance_test $index))



m2<-lm(expenses~ age + highBMIsmoke + highBMI + malesmoker + 
         smoker + agesmoker + children + southeastsmoke + highBMIsmokeage+ southwestsmoke + parentsmoke + region + sex, data = insurance_test)



summary(m2)
rmse(m2, insurance_test)


