library(MASS)
library(ISLR)
library(caTools)

setwd("E:/NMIMS Mtech (Sem-II)/Data Science - II/DS(Test 2)")
dataset<- read.csv("M2_test.csv", stringsAsFactors = T, header = T)
head(dataset)
str(dataset)

split<- sample.split(dataset, SplitRatio = 0.8)
training<- subset(dataset, split=T)
testing<- subset(dataset, split=F)
testing_direction<- testing$yes

# Fit the logistic model on training dataset
logistic_model<- glm(dataset$yes~., training, family = "binomial")
summary(logistic_model)

# predict based on testing data
model_pred_prob<- predict(logistic_model, testing, type = "response")
model_pred_direction<- model_pred_prob>0.5

# Confusion Matrix
confusion<- table(model_pred_direction,testing_direction)

#Accuracy
sum(diag(confusion))/sum(confusion)

# misclassification error
mean(model_pred_direction != testing_direction)


###### The Lasso ##################################
library(glmnet)
lasso.mod=glmnet(training,training$yes,alpha=1,lambda=grid, family = "binomial")
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]