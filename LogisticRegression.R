setwd("~/Desktop/Open IIT Analytics Competition")
train <- read.csv("~/Desktop/Open IIT Analytics Competition/train.csv")
#View(train)
test <- read.csv("~/Desktop/Open IIT Analytics Competition/test.csv")
#View(test)
#str(train) #To have a look at the structure of the input data-frame
mylogit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train3, family = "binomial") #Preparing a logistic Regression Model
summary(mylogit)
confint.default(mylogit)
test3$PassengerId <- NULL
test3$Name <- NULL
test3$Ticket <- NULL
test3$Cabin<-NULL
test3$Survived <- predict(mylogit, newdata = test3, type = "response")
test3$Survived<-ifelse(test3$Survived>0.5,1,0)
submit=data.frame(PassengerId="test$PassengerId",Survived="test$Survived")
write.csv(submit, file = "theyallperish.csv", row.names = FALSE) # The write.csv command has sent that dataframe out to a CSV file, and importantly excluded the row numbers.

