library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

library("ggcorrplot")
library("ROCR")
# Load dataset
db = read.csv('diabetes.csv', header=TRUE)
# Understand the structure of the dataset
str(db)

summary(db)
# Create Age Category column
db$Age_Cat <- ifelse(db$Age < 21, "<21", 
                     ifelse((db$Age>=21) & (db$Age<=25), "21-25", 
                            ifelse((db$Age>25) & (db$Age<=30), "25-30",
                                   ifelse((db$Age>30) & (db$Age<=35), "30-35",
                                          ifelse((db$Age>35) & (db$Age<=40), "35-40",
                                                 ifelse((db$Age>40) & (db$Age<=50), "40-50",
                                                        ifelse((db$Age>50) & (db$Age<=60), "50-60",">60")))))))

db$Age_Cat <- factor(db$Age_Cat, levels = c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))
table(db$Age_Cat)

# Histogram of Age

ggplot(aes(x = Age), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")
# Barplot by Age_Cat
ggplot(aes(x = Age_Cat), data = db) +
  geom_bar(fill='steelblue')

# box plot of Age_Cat vs BMI
ggplot(aes(x=Age_Cat, y = BMI), data = db) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,70))
by(db$BMI, db$Age_Cat, summary)
# Compute correlation matrix
db_cor <- round(cor(db[1:8]),1)
db_cor  

ggcorrplot(db_cor)

# Split dataset into train and test sets
requirere(cachemaTools)
set.seed(3)
sample = sample.split(db$Outcome, SplitRatio=0.75)
train = subset(db, sample==TRUE)
test = subset(db, sample==FALSE)
nrow(db)
nrow(train)
nrow(test)
# distribution of Age category in Train set
table(train$Age_Cat)

# Baseline model
table(db$Outcome)

# Fit model - using all independent variables

AllVar <- glm(Outcome ~ ., data = train, family = binomial)
summary(AllVar)
#outcome on Training dataset

PredictTrain <- predict(AllVar, type = "response")
summary(PredictTrain)
# This computes the average prediction for each of the two outcomes

tapply(PredictTrain, train$Outcome, mean)
# Build confusion matrix with a threshold value of 0.5

threshold_0.5 <- table(train$Outcome, PredictTrain > 0.5)
threshold_0.5

# Accuracy
accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
sprintf("Accuracy is %s",accuracy_0.5)



# Accuracy
accuracy_0.7 <- round(sum(diag(threshold_0.7))/sum(threshold_0.7),2)
sprintf('Accuracy is %s', accuracy_0.7)


# Making predictions on test set

PredictTest <- predict(AllVar, type = "response", newdata = test)

# Convert probabilities to values using the below

##  selected a threshold of 0.5
test_tab <- table(test$Outcome, PredictTest > 0.5)
test_tab

accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Accuracy on test set is %s", accuracy_test)
# Compute test set AUC

ROCRPredTest = prediction(PredictTest, test$Outcome)
auc = round(as.numeric(performance(ROCRPredTest, "auc")@y.values),2)
auc

