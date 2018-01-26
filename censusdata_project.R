path="/Users/shruti/Downloads/"
setwd(path)
install.packages("data.table")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("bindrcpp")
install.packages("plotly")
install.packages("caret")
install.packages("mlr")
install.packages("FSelector")
install.packages("ggvis")
install.packages("RWekajars")
install.packages("rJava",type = "source")
install.packages("RWeka")
install.packages("e1071")
install.packages("xgboost")

library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(bindrcpp)
library(ggplot2)
library(plotly)
library(caret)
library(mlr)
library(rJava)
library(RWeka)
library(FSelector)
library(RWekajars)
library(e1071)
library(ggvis)
library(xgboost)


train=fread("train.csv", na.strings = c(""," ","?","NA",NA))
test=fread("test.csv", na.strings = c(""," ","?","NA",NA))
View(train)
View(test)
str(train)
str(test)
unique(test$income_level)
train[,income_level:=ifelse(income_level=="-50000",0,1)]
test[,income_level:=ifelse(income_level=="-50000",0,1)]
round(prop.table(table(train$income_level))*100)

#set column classes
factcols=c(2:5,7,8:16,20:29,31:38,40,41)
numcols=setdiff(1:40,factcols)

#coerce to factor and numeric variables as per data set
train %<>% mutate_at(factcols, funs(factor(.)))
train %<>% mutate_at(numcols, funs(as.numeric))
train=as.data.table(train)
#train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
#train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
#subset categorical and numeric train variables
cat_train=train[,factcols,with=FALSE]
num_train=train[,numcols,with=FALSE]

#coerce to factor and numeric variables as per data set
test %<>% mutate_at(factcols, funs(factor(.)))
test %<>% mutate_at(numcols, funs(as.numeric))
test=as.data.table(test)
#test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
#test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
#subset categorical and numeric test variables
cat_test=test[,factcols,with=FALSE]
num_test=test[,numcols,with=FALSE]

str(cat_test)
str(num_test)

tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

#add response var in num_train data to study effect of num vars on response vars
num_train[,income_level := cat_train$income_level]

#create a scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

#dodged bar chart for categorical variables
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_bar(cat_train$education)
all_bar(cat_train$class_of_worker)

#proportionate tables to check the effect of dependent variable per categories
prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)

#check missing values in numerical data
table(is.na(num_train))
table(is.na(num_test))

#set threshold as 0.7 to remove vars having high correlation
num_train[,income_level:=NULL]
ax<-findCorrelation(cor(num_train), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE]
num_test <- num_test[,-ax,with=FALSE]

#check missing values per columns of categorical data
mvtrain<-sapply(cat_train, function(x){sum(is.na(x))/length(x)*100})
mvtest<-sapply(cat_test, function(x){sum(is.na(x))/length(x)*100})
#select columns with less than 5% missing values and remove those vars having more than 5%
cat_train <- subset(cat_train, select = mvtrain < 5 )
cat_test <- subset(cat_test, select = mvtest < 5 )

#set NA as Unavailable for train data
#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable for test data
#convert to characters
cat_test<- cat_test[,names(cat_test) := lapply(.SD, as.character),.SDcols = names(cat_test)]
for(i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])),j = i, value = "Unavailable")
#convert back to factors
cat_test <- cat_test[, names(cat_test) := lapply(.SD,factor), .SDcols = names(cat_test)]

#combine factor levels with less than 5% values for train data
rm(train)
rm(test)
for(i in names(cat_train))
{
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test))
{
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

#test if cat vars in test and train have same levels
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]
num_train[,.N, capital_gains][order(-N)]
num_train[,.N,capital_losses][order(-N)]
num_train[,.N,num_person_Worked_employer][order(-N)]
num_train[,.N,dividend_from_Stocks][order(-N)]

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero since most of the values are 0
num_train[,wage_per_hour:=ifelse(wage_per_hour==0,"Zero","More than 0")]
num_train[,wage_per_hour:=as.factor(wage_per_hour)]
num_train[,capital_gains:=ifelse(capital_gains==0,"Zero","More than 0")][,capital_gains:=as.factor(capital_gains)]
num_train[,capital_losses:=ifelse(capital_losses==0,"Zero","More than 0")][,capital_losses:=as.factor(capital_losses)]
num_train[,dividend_from_Stocks:=ifelse(dividend_from_Stocks==0,"Zero","More than 0")][,dividend_from_Stocks:=as.factor(dividend_from_Stocks)]
num_train[,num_person_Worked_employer:=ifelse(num_person_Worked_employer==0,"Zero","More than 0")][,num_person_Worked_employer:=as.factor(num_person_Worked_employer)]


num_test[,wage_per_hour:=ifelse(wage_per_hour==0,"Zero","More than 0")]
num_test[,wage_per_hour:=as.factor(wage_per_hour)]
num_test[,capital_gains:=ifelse(capital_gains==0,"Zero","More than 0")][,capital_gains:=as.factor(capital_gains)]
num_test[,capital_losses:=ifelse(capital_losses==0,"Zero","More than 0")][,capital_losses:=as.factor(capital_losses)]
num_test[,dividend_from_Stocks:=ifelse(dividend_from_Stocks==0,"Zero","More than 0")][,dividend_from_Stocks:=as.factor(dividend_from_Stocks)]
num_test[,num_person_Worked_employer:=ifelse(num_person_Worked_employer==0,"Zero","More than 0")][,num_person_Worked_employer:=as.factor(num_person_Worked_employer)]

#Combine num and cat data sets
d_train<-cbind(num_train,cat_train)
d_test<-cbind(num_test,cat_test)
#remove unwanted files to save memory
rm(num_train,num_test,cat_train,cat_test)
#create classification task
train.task<-makeClassifTask(data=d_train, target="income_level") #in mlr package
test.task<-makeClassifTask(data=d_test, target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#undersampling 
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))

#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

#synthetic data generation using smote which in turn uses k-nn
system.time(train.smote<-smote(train.task,nn=3, rate = 10))
table(getTaskTargets(train.smote))

#available algorithms for classification
listLearners("classif","twoclass")[c("class","package")]

#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

fun_cv (train.task) 
fun_cv (train.under)
fun_cv (train.over) 
fun_cv (train.smote) 

#train and predict
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)

#evaluate performance metrics
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)

#calculate F measure
precision <- dCM$byClass['Precision']
recall <- dCM$byClass['Sensitivity']
f_measure <- 2*((precision*recall)/(precision+recall))
f_measure 

#xgboost
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)

#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = list(max_depth=3, lambda=0.221, eta=0.161, subsample=0.698, min_child_weight=7.67, colsample_bytree=0.642))

#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)
precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']
f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

#naivebayes AUC 
nb_prob <- setPredictType(learner = naive_learner,predict.type = "prob")

#train model
nbmodel_prob <- train(nb_prob,train.task)

#predict
predict.nbprob <- predict(nbmodel_prob,test.task)

#predicted probabilities
predict.nbprob$data[1:10,]

df <- generateThreshVsPerfData(predict.nbprob,measures = list(fpr,tpr))
plotROCCurves(df)

#set threshold as 0.4
pred2 <- setThreshold(predict.nbprob,0.4)
confusionMatrix(d_test$income_level,pred2$data$response)

#set threshold as 0.3
pred3 <- setThreshold(predict.nbprob,0.3)
confusionMatrix(d_test$income_level,pred3$data$response)

#SVM
getParamSet("classif.svm")
svm_learner <- makeLearner("classif.svm",predict.type = "response")
svm_learner$par.vals<- list(class.weights = c("0"=1,"1"=10),kernel="radial")

svm_param <- makeParamSet(
  makeIntegerParam("cost",lower = 10^-1,upper = 10^2), 
  makeIntegerParam("gamma",lower= 0.5,upper = 2)
)

#random search
set_search <- makeTuneControlRandom(maxit = 5L) #5 times

#cross validation #10L seem to take forever
set_cv <- makeResampleDesc("CV",iters=5L,stratify = TRUE)

#tune Params
svm_tune <- tuneParams(learner = svm_learner,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = svm_param,control = set_search,resampling = set_cv)

#set hyperparameters
svm_new <- setHyperPars(learner = svm_learner, par.vals = svm_tune$x)

#train model
svm_model <- train(svm_new,train.task)

#test model
predict_svm <- predict(svm_model,test.task)

confusionMatrix(d_test$income_level,predict_svm$data$response)
