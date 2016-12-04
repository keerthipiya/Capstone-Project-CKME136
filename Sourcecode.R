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

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)


# Look at age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies

# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived, class=test$Pclass)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)



# Install and load required packages for fancy decision tree plotting
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Recreate the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

fit1<-rpart(Survived ~ Pclass + Child, data =train, method="class")
fancyRpartPlot(fit1)

agtrain$Survived ~ train$Pclass + train$Child

# Build a deeper tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Plot it with base-R
plot(fit)
text(fit)

# And then make it look better with fancyRpartPlot!
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Let's unleash the decision tree and let it grow to the max
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfullgrowntree.csv", row.names = FALSE)

# Manually trim a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)



# What's in a name?
train$Name[1]

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title
combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Won't work!
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)

library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)


library(caret)
install.packages("caret")
library(kernlab)
install.packages("kernlab")

Train<-read.csv("C:/Users/keerthi/Desktop/kaggle/train.csv")
Test<-read.csv("C:/Users/keerthi/Desktop/kaggle/test.csv")
rm(list=ls())
cat("\014")

#### Preprocessing data ####
data.raw <- read.csv("C:/Users/keerthi/Desktop/kaggle/train.csv",na.strings="",
                     colClasses=c('integer','factor','factor','character',
                                  'factor','numeric','factor','factor',
                                  'character','numeric','factor','factor'))



datatrain<-Train
datatrain$Age<-NULL
kc<-kmeans(datatrain,3)


table(datatrain$Age, kc$cluster)

#### Preparing for Training ####
set.seed(5425)
InTrain <- createDataPartition(data1$Age, p=0.75,list=FALSE)
createDataPartition(kdata)

dat<-is.na(Train$Age)
inTrain<-createDataPartition(dat, p=0.75, list = FALSE)


data.train <- data[-inTrain,]
dim(data.train)

data.cv <- data[-InTrain,]
dim(data.cv)


#### Training Model 0: Logistic Regression ####
set.seed(1521)
modFit.logit <- glm(Survived ~ ., data=data.train, family=binomial(logit))
summary(modFit.logit)

data.cv.lda <- data.cv
data.cv.lda$Pclass <- as.numeric(data.cv.lda$Pclass)
data.cv.lda$SibSp <- as.numeric(data.cv.lda$SibSp)
data.cv.lda$Parch <- as.numeric(data.cv.lda$Parch)
modPred.lda <- predict(modFit.lda, newdata=data.cv.lda, method='predictive')
confusionMatrix(table(modPred.lda$class,data.cv.lda$Survived))



#### Training Model 5: Support Vector Machine Model ####
modFit.svm <- svm(Survived ~ ., data=data.train)
modPred.svm <- predict(modFit.svm, newdata=data.cv)
confusionMatrix(table(modPred.svm,data.cv$Survived))
# Confusion Matrix and Statistics
  

#### Aggregating the six models using random forest model ####
modPred.logit <- as.factor(modPred.logit) # Accuracy : 0.763 
modPred.rpart <- as.factor(modPred.rpart) # Accuracy : 0.763
modPred.rf    <- as.factor(modPred.rf)    # Accuracy : 0.7725
modPred.knn3  <- as.factor(modPred.knn3)  # Accuracy : 0.6398
modPred.lda   <- as.factor(modPred.lda$class) # Accuracy : 0.7678
modPred.svm   <- as.factor(modPred.svm)   # Accuracy : 0.7725
modPreds <- data.frame(logit=modPred.logit, rpart=modPred.rpart,
                       rf   =modPred.rf,    knn3 =modPred.knn3,
                       lda  =modPred.lda,   svm  =modPred.svm,
                       Survived=data.cv$Survived)
library(randomForest)
modFit.Agg  <- randomForest(Survived ~ ., data=modPreds)
# Call:
#     randomForest(formula = Survived ~ ., data = modPreds) 
#


#### Aggregating the six models using majority vote model ####
modPred.logit <- as.numeric(modPred.logit) # Accuracy : 0.763 
modPred.rpart <- as.numeric(modPred.rpart) # Accuracy : 0.763
modPred.rf    <- as.numeric(modPred.rf)    # Accuracy : 0.7725
modPred.knn3  <- as.numeric(modPred.knn3)  # Accuracy : 0.6398
modPred.lda   <- as.numeric(modPred.lda)   # Accuracy : 0.7678
modPred.svm   <- as.numeric(modPred.svm)   # Accuracy : 0.7725

modPred.majv  <- modPred.logit + modPred.rpart + modPred.rf + 
  modPred.knn3 + modPred.lda + modPred.svm

modPreds      <- ifelse(modPred.majv > 9, 1, 0)
confusionMatrix(table(modPreds,data.cv$Survived))
# Confusion Matrix and Statistics

#4.	Identify the class of the survivals and died?
table(train$Pclass, train$Sex)
table(train$Pclass,train$Survived)
#5.	What is my understanding from research?
