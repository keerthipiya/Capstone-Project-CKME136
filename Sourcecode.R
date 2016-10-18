#set working directory and import files
setwd("C:/Users/keerthi/Desktop/kaggle")
test <- read.csv("C:/Users/keerthi/Desktop/kaggle/test.csv")
View(test)

train <- read.csv("C:/Users/keerthi/Desktop/kaggle/train.csv")
View(train)


#variable names list
variable.names(train)
variable.names(test)

#listing objects and strcutre
str(train)
str(test)

#Research questions:
#1.	How many of them survived form the disaster?

train<- read.csv("train.csv", stringsAsFactors = FALSE)
table(train$Survived)


#2.	Percentage of women and men survival?
train<- read.csv("train.csv", stringsAsFactors = FALSE)
table(train$Survived)

#perentage of survival
prop.table(table(train$Survived))
#table for gender survivals
table(train$Sex,train$Survived)
#percentage of female and male survival
prop.table(table(train$Sex,train$Survived),1)

#3.	Find the survivals of their age and gender?
table(train$Age,train$Sex)
summary(train$Age)


#4.	Identify the class of the survivals and died?
table(train$Pclass, train$Sex)
table(train$Pclass,train$Survived)
#5.	What is my understanding from research?
