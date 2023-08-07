library(tidyverse) 
library(readxl)
library(ggplot2)
library(dplyr)
library(mltools)
install.packages('mltools')
library(data.table)

#Data Summary
data<- read_xlsx("Documents/UTD/SEM-3/Predictive/Project/data2.xlsx")

#Checking na 
colSums(is.na(data))

#dimensions
names(data)
dim(data)

#binary variables
data$Attrition <- ifelse(data$Attrition=='Yes',1,0)
data$Gender <- ifelse(data$Gender=='Female',1,0)
data$OverTime <- ifelse(data$OverTime=='Yes',1,0)

##dropping variables- employee count, employee number, over18
data <- select(data,-c(EmployeeCount,EmployeeNumber,Over18,StandardHours))

##one-hot encoding of variables


cols <- c("BusinessTravel", "Department", "Education", "EducationField", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")
data[cols] <- lapply(data[cols], as.factor)

# sapply(data, class) # to check datatype of all fields

df.table <- as.data.table(data)
final_data <- one_hot(df.table)
final_data <- select(final_data,-Age)

# dividing the data into test & training sample
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(final_data), replace=TRUE, prob=c(0.7,0.3))
train  <- final_data[sample, ]
test   <- final_data[!sample, ] 

## logistic regression model

model <- glm(Attrition ~ .,data=train,family='binomial')
summary(model)

test$Attrition_prob <- model %>% predict(test, type='response')
head(test$Attrition_prob)

test$Attrition_pred <- ifelse(test$Attrition_prob >= 0.5, 1, 0)
head(test$Attrition_pred)

sum(test$Attrition == test$Attrition_pred)

accuracy_prediction <- (sum(test$Attrition == test$Attrition_pred)/dim(test)[1])
