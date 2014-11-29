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
#Because we built the factors on a single dataframe, and then split it apart after we built them, R will give all factor levels to both new dataframes, even if the factor doesn’t exist in one
#Subsetting a dataframe
train4 <- combi[1:891,]
test4 <- combi[892:1309,]
#The comma after that with no numbers following it indicates that we want to take ALL columns with this subset and store it to the assigned dataframe
#Build Decision tree Model
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyI,data=train4, method="class")
#Plotting Decision Tree Model
plot(fit)
text(fit)
#Installing and Importing some libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#Fancy plot of Decision Tree
fancyRpartPlot(fit)
#Decision Trees are biased to favour factors with many level
#Predicting
Prediction <- predict(fit, test4, type = "class")

                      




