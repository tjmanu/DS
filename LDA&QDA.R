library(MASS)
library(ISLR)
library(caTools)
library(e1071)
library(caret)

data_iris<- iris
head(data_iris)
str(data_iris)

split<- sample.split(data_iris, SplitRatio = 0.8)
training<- subset(data_iris, split=T)
testing<- subset(data_iris, split=F)
testing_direction<- testing$Species

# fit lda model based on training data
lda_model<- lda(data_iris$Species~., data = training)
lda_model
lda_cutoff<- lda_model$svd               # cut-off Z1 and Z2

# Validate the lda model using testing data
lda_pred<- predict(lda_model, testing)
lda_pred_direction<- lda_pred$class

# Confusion Matrix
table(lda_pred_direction, testing_direction)
confusionMatrix(lda_pred_direction,testing_direction)

# misclassification error
mean(lda_pred_direction != testing_direction)


##### QDA
qda_model<- qda(data_iris$Species~., data = training)
qda_model

qda_pred<- predict(qda_model, testing)
qda_pred_direction<- qda_pred$class
table(qda_pred_direction,testing_direction)
confusionMatrix(qda_pred_direction,testing_direction)
mean(qda_pred_direction != testing_direction)

#### LDA misclassification error : 2%
#### QDA misclassification error : 2%