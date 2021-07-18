install.packages("AppliedPredictiveModeling") 
install.packages("zoo") 
install.packages("sandwich") 
install.packages("mvtnorm") 
install.packages("stats4") 
install.packages("modeltools") 
install.packages("strucchange")
install.packages("ggplot2") 
install.packages("heuristica")
install.packages("arulesCBA")
install.packages("party")
install.packages("magrittr") 

library(outliers)
library(magrittr)
library(caret)
library(tidyverse)
library(lubridate)
library(caTools)
library(partykit) 
library(e1071)


#read the data set
Pdataset= read.csv('Admission_Predict.csv')


names(Pdataset)
print('Number missing vlaues for each attributes')
sum(is.na(Pdataset$Chance.of.Admit))
sum(is.na(Pdataset$GRE.Score))
sum(is.na(Pdataset$TOEFL.Score))
sum(is.na(Pdataset$University.Rating))
sum(is.na(Pdataset$SOP))
sum(is.na(Pdataset$LOR))
sum(is.na(Pdataset$CGPA))
sum(is.na(Pdataset$Research))
sum(is.na(Pdataset$Chance.of.Admit))

str(Pdataset)


summary(Pdataset$TOEFL.Score)
summary(Pdataset$GRE.Score)
summary(Pdataset$University.Rating)
summary(Pdataset$SOP)
summary(Pdataset$LOR)
summary(Pdataset$CGPA)
summary(Pdataset$Chance.of.Admit)


pie(table(Pdataset$University.Rating))
hist(Pdataset$CGPA)
with(Pdataset,plot(Chance.of.Admit,TOEFL.Score))


boxplot(Pdataset$TOEFL.Score)
boxplot(Pdataset$GRE.Score)
boxplot(Pdataset$SOP)
boxplot(Pdataset$LOR)
boxplot(Pdataset$CGPA)
boxplot(Pdataset$Chance.of.Admit)


#data reprocessing (cleaning & transformation)

#1-missing value
is.na(Pdataset)
sum(is.na(Pdataset))

#2-Finding and deleting the outliers

outl1=outlier(Pdataset$GRE.Score,logical = TRUE)
sum(outl1)

outl2=outlier(Pdataset$TOEFL.Score,logical = TRUE)
sum(outl2)

outl3=outlier(Pdataset$University.Rating,logical = TRUE)
sum(outl3)

outl4=outlier(Pdataset$SOP,logical = TRUE)
sum(outl4)

outl5=outlier(Pdataset$LOR,logical = TRUE)
sum(outl5)

outl6=outlier(Pdataset$CGPA,logical = TRUE)
sum(outl6)

outl7=outlier(Pdataset$Chance.of.Admit,logical = TRUE)
sum(outl7)

Find_outlier= which(outl1==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl2==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl3==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl4==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl5==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl6==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]

Find_outlier= which(outl7==TRUE, arr.ind = TRUE)
Pdataset=Pdataset[-Find_outlier,]


#correlation 
cor(Pdataset[,2:8])

#3-factor
is.factor(Pdataset$University.Rating)                 
is.numeric(Pdataset$University.Rating)

Pdataset$University.Rating = factor(Pdataset$University.Rating,levels = c(1,2,3,4,5),labels = c("Bad","Okay","Good","Great","Excellent"))
#Checking
is.factor(Pdataset$University.Rating)                 
is.numeric(Pdataset$University.Rating)

#4-normalization and scale
normalize<-function(x) {return((x-min(x))/max(x)-min(x))}
Pdataset[,3]=scale(Pdataset[,3])




#---------- phase2 ---------#

# transform Chance.of.Admit attribute into binary variable
Pdataset$Chance.of.Admit <- as.factor(ifelse(Pdataset$Chance.of.Admit  > 0.5, 1, 0))

# remove Serial.No column
Pdataset$Serial.No. <- NULL
data <- Pdataset

# setup settings1
set.seed(300)
ind <- sample(2, nrow(data) , replace = TRUE , prob = c(0.7 , 0.3))
tr <- data[ind == 1,]
ts <- data[ind == 2,]

t1 <- ctree(Chance.of.Admit ~ .,  data = tr) 
r1 <- predict(t1,ts[,-grep('Chance.of.Admit',names(data))],type='response')
confusionMatrix(as.factor(r1), as.factor(ts$Chance.of.Admit))
plot(t1)


# setup settings2
set.seed(300)
ind <- sample(2, nrow(data) , replace = TRUE , prob = c(0.8 , 0.2))
tr <- data[ind == 1,]
ts <- data[ind == 2,]

t2 <- ctree(Chance.of.Admit ~ .,  data = tr) 
r2 <- predict(t2,ts[,-grep('Chance.of.Admit',names(data))],type='response')
confusionMatrix(as.factor(r2), as.factor(ts$Chance.of.Admit))
plot(t2)


# setup settings3
set.seed(300)
ind <- sample(2, nrow(data) , replace = TRUE , prob = c(0.6 , 0.4))
tr <- data[ind == 1,]
ts <- data[ind == 2,]


t3 <- ctree(Chance.of.Admit ~ .,  data = tr) 
r3 <- predict(t3,ts[,-grep('Chance.of.Admit',names(data))],type='response')
confusionMatrix(as.factor(r3), as.factor(ts$Chance.of.Admit))
plot(t3)



