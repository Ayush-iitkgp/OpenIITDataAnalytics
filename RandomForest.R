setwd("~/Desktop/Open IIT Analytics Competition")
train4 <- read.csv("~/Desktop/Open IIT Analytics Competition/train.csv")
test4 <- read.csv("~/Desktop/Open IIT Analytics Competition/test.csv")
test4$Name[1]
test4$Survived <- NA
combi <- rbind(train4, test4)
#Strings are automatically imported as factors in R
combi$Name <- as.character(combi$Name)
combi$Name[1]
#We can easily use the function strsplit, which stands for string split
strsplit(combi$Name[1], split='[,.]')
#String split uses a doubly stacked matrix because it can never be sure that a given regex will have the same number of pieces
strsplit(combi$Name[2], split='[,.]')[[1]][2]
combi$Title<-strsplit(combi$Name, split='[,.]')
#R has some extremely useful functions that apply more complication functions one row at a time.
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
#we can just substitute the first occurrence of a space with nothing. We can use sub for this 
combi$Title <- sub(' ', '', combi$Title)
# Mademoiselle and Madame are pretty similar (so long as you don’t mind offending) so let’s combine them into a single category
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#The %in% operator checks to see if a value is part of the vector we’re comparing it to
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
#Large family may have issues getting to lifeboats together, 
combi$FamilySize <- combi$SibSp + combi$Parch + 1
#Well we just thought about a large family having issues getting to lifeboats together, but maybe specific families had more trouble than others? We could try to extract the Surname of the passengers and group them to find families.
#Combining the Surname with the family size though should remedy this concern
#Extract the passenger's last name
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#Append the FamilySize variable to the front of Surname using paste method
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
#Knock out any family size of two or less and call it a “small” family
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
#Create a new frame consisting of columns FamilyID and its frequency
# We can store most tables to a dataframe
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
#need to overwrite any family IDs in our dataset for groups that were not correctly identified 
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
summary(combi$Age)
library(rpart)
#Predicting the missing values of Age using Decision Trees
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)
summary(combi$Embarked)
#Two Embarked are blank , we need to find out which are they
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
#Our FamilyID variable had almost double that. We could take two paths forward here, either change these levels to their underlying integers (using the unclass() function) and having the tree treat them as continuous variables, or manually reduce the number of levels to keep it under the threshold
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
#Bcoz the algorithm has two source of Randomness so to reproduce the result next time , we set the seed
set.seed(415)
train5 <- combi[1:891,]
test5 <- combi[892:1309,]

#Installing Random Forest
#install.packages('randomForest')
#Importing Random Forest
library(randomForest)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train5, importance=TRUE, ntree=2000)
#Plotting to see which variable is important
varImpPlot(fit)
#Predict using random Forest
Prediction <- predict(fit, test5)
submit5 <- data.frame(PassengerId = test5$PassengerId, Survived = Prediction)
write.csv(submit5, file = "RandomForest.csv", row.names = FALSE)
#Install and load "party" package
#install.packages('party')
library(party)
#Set seed 
set.seed(415)
#Build conditional inference Model , obseve that the parameters are passed in different way in this model
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train5, controls=cforest_unbiased(ntree=2000, mtry=3))
#Predicting
Prediction <- predict(fit, test5, OOB=TRUE, type = "response")
submit5_2 <- data.frame(PassengerId = test5$PassengerId, Survived = Prediction)
write.csv(submit5_2, file = "ConditionalInferenceTree.csv", row.names = FALSE)
train5$Cabin<-as.character(train5$Cabin)
train5$Cabin <- sapply(train5$Cabin, FUN=function(x) {strsplit(x, split='')[[1]][1]})
train5$Cabin[(is.na(train5$Cabin))]<-'b'
test5$Cabin<-as.factor(test5$Cabin)
train5$Cabin<-as.factor(train5$Cabin)
