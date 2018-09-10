#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/new-thyroid.data"
thyroid <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
#Getting dimens,ons names
dimnames(thyroid)
#Change row and column names
rownames(thyroid) <- paste0 ("Patient", seq(from = 1, to = nrow(thyroid), by = 1))
colnames(thyroid) <- c("Diagnosis", "RT3U", "T4", "T3", "TSH", "DTSH")
#Summary statistics by visualization
boxplot(thyroid)
pairs(~T3+T4+TSH+Diagnosis, data=thyroid)
#Adjusting Target Variable
thyroid[1] <- as.factor(x = thyroid [[1]])
install.packages ("plyr")
library(plyr)
thyroid$Diagnosis <- revalue(thyroid$Diagnosis, c("1" = "otiroid", "2" = "hiper" , "3" = "hipo"))
#Installing Caret Package
install.packages("caret")
library(caret)
#Data Partitioning
set.seed(123)
trainIndices <- createDataPartition(y = thyroid$Diagnosis, p=0.7, list = FALSE)
trainset <- thyroid [trainIndices, ]
testset <- thyroid [-trainIndices, ]
# Installing Weka Package for C4.5 Algorithm
install.packages("RWeka")
library(RWeka)
# Applying J48 from RWeka package
C4.5 <- J48(Diagnosis ~., data=trainset)
show(C4.5)
#Summary statistics of our Model
summary(C4.5)
#Visualization of Model
install.packages('partykit')
library(partykit)
plot(C4.5)
# Making predictions
predictions <- predict(C4.5, testset, type="class")
show(predictions)
# Creating Confusion Matrix
cm <- table(testset$Diagnosis, predictions, dnn = c("Real Values", "Predictions"))
show(cm)

#Calculating Accuracy
paste ("Accuracy = ", (accuracy <- (43+9+7)/sum(cm)))

