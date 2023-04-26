library(Amelia)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)
set.seed(3)

###titanic dataset

train_set <- read.csv("C:/Users/raypo/OneDrive/Desktop/homework-4-rayell86-update/titanic3.csv")
train_set <- select(train_set,-body)
sum(is.na(train_set$age))
#https://www.kaggle.com/datasets/vinicius150987/titanic3?select=titanic3.xls

#lets explore the training set first

#look for missing data (need Amelia package)
missmap(train_set,main = "missing map", col = c("red","black"),legend=FALSE) 
#about 2% of the data in the age ctegory is missing. 
# in order to replace the missing values with meaningful data, I want to look at the average age of passengers and compare them to their class.
# this allows us to understand the mean age of passengers and their class therefore, we can replace missing age values based on what class they were in.
#in order to understand the mean of age, I use a box plot.

#determine number of NAs for age

(sum(is.na(train_set))/nrow(train_set))


train_set %>%
  ggplot(aes(pclass,age,fill=factor(pclass)))+
  geom_boxplot(aes(group=pclass))+
  scale_y_continuous(breaks = seq(min(0),max(85),2))+
  ggtitle("Age by Class Distribution")

#here we can see that the average age for passengers in class 1 was 40, class 2 was 30 and class 3 was 25.
#based on this break down, I will write a variable that associates age of missing people with  the class that they were in.


#############
age_fun <- function(age,class){
  out <- age
  for (i in 1:length(age)) {
    if (is.na(age[i])){
      if (class[i]==1){
        out[i] <- 40
      }else if(class[i]==2){
        out[i] <- 30
      }else{
        out[i] <- 25
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
#####

train_set <- train_set %>%
  mutate(new_age=ifelse(is.na(age)==TRUE&pclass==1,40,
                        ifelse(is.na(age)==TRUE&pclass==2,30,
                               ifelse(is.na(age)==TRUE&pclass==3,25,age))))


train_set <- train_set %>%
  mutate(survived=factor(survived),pclass=factor(pclass),parch=factor(parch),sibsp=factor(sibsp)) %>%
  select(-c(cabin,ticket,name,home.dest,age,boat,cabin,embarked))

train_set <- rename(train_set,age=new_age)

train_set <- train_set%>%na.omit()


#check survival count
ggplot(train_set, aes(survived,fill=factor(survived))) + geom_bar() + ggtitle("Survival Count") 

#check passengers' class
ggplot(train_set, aes(pclass,fill=factor(survived))) + geom_bar() + ggtitle("Passengers' Survival Count By Ticket Type") 

#check passengers' gender
ggplot(train_set, aes(sex,fill=factor(survived))) + geom_bar() + ggtitle("Passengers' Survival Count By Gender") 

#check passengers' age range
ggplot(train_set,aes(age,fill=factor(survived))) + geom_histogram(bins=20) + ggtitle("Passengers' Survival Count By Age")

######
#getting the data set ready for analysis
######
#selecting variables needed for my analysis



formula1 <- survived~.
formula2 <- survived~age+sex
formula3 <- survived~age+sex+sibsp+parch+pclass

summary(train_set %>%glm(formula1,family=binomial(link="logit"),data=.))

summary(train_set %>%glm(formula2,family=binomial(link="logit"),data=.))

summary(train_set %>%glm(formula3,family=binomial(link="logit"),data=.))


#seting test and training set

y <- train_set$survived

test_data <- createDataPartition(y,times = 1,p=.7,list=FALSE)

train_df <- train_set[test_data,]
test_df <- train_set[-test_data,]

model <- glm(formula1,family=binomial(link="logit"),data=train_df)

summary(model)

test_1 <- predict(model,test_df,type = "response")
trim_test_1 <- ifelse(test_1<.55,0,1)

confusionMatrix(factor(trim_test_1),factor(test_df$survived))

#####use a decision tree model (Random Forest)

library(rpart)
library(rpart.plot)
library(randomForest)

tree_model1 <- rpart(survived~.,method="class",data=train_df)
summary(tree_model1)

pred_1 <- predict(tree_model1,test_df)
pred_1 <- as.data.frame(pred_1)
pred_1$compass <- ifelse(pred_1$`1`>=.5,1,0)

table(pred_1$compass,test_df$survived)

prp(tree_model1)



forest_mod1 <- randomForest(survived~., data=train_df,importance=TRUE)
forest_mod1$confusion

pred_2 <- predict(forest_mod1,test_df)

table(pred_2,test_df$survived)



