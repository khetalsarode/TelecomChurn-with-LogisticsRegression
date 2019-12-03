#DATA PREPARATION
telecom<-read.csv("telecom churn.csv")
summary(telecom)
telecom<-telecom[-c(1,3,4)]
summary(telecom)

#MISSING VALUE TREATEMENT
hist(telecom$number.vmail.messages)
telecom$number.vmail.messages<-ifelse(is.na(telecom$number.vmail.messages),0,telecom$number.vmail.messages)
hist(telecom$total.day.calls)
telecom$total.day.calls<-ifelse(is.na(telecom$total.day.calls),100,telecom$total.day.calls)
hist(telecom$total.day.charge)
telecom$total.day.charge<-ifelse(is.na(telecom$total.day.charge),30.56,telecom$total.day.charge)
telecom$total.eve.calls<-ifelse(is.na(telecom$total.eve.calls),100,telecom$total.eve.calls)
telecom$total.eve.charge<-ifelse(is.na(telecom$total.eve.charge),17,telecom$total.eve.charge)
telecom$total.night.calls<-ifelse(is.na(telecom$total.night.calls),100,telecom$total.night.calls)
telecom$total.night.charge<-ifelse(is.na(telecom$total.night.charge),9,telecom$total.night.charge)
hist(telecom$total.intl.calls)
telecom$total.intl.calls<-ifelse(is.na(telecom$total.intl.calls),4,telecom$total.intl.calls)
telecom$total.intl.charge<-ifelse(is.na(telecom$total.intl.charge),2.78,telecom$total.intl.charge)
hist(telecom$customer.service.calls)
telecom$customer.service.calls<-ifelse(is.na(telecom$customer.service.calls),1,telecom$customer.service.calls)
summary(telecom)

#DUMMY VARIABLE CFREATION
telecom$international.plan<-ifelse(telecom$international.plan=="no",0,1)
telecom$voice.mail.plan<-ifelse(telecom$voice.mail.plan=="no",0,1)
telecom$churn<-ifelse(telecom$churn=="FALSE",0,1)
summary(telecom)


#ANALYZE CORELATION BETWEEN DEPENDENT VARIABLE CHURN AND OTHER VARIABLES
scatterplot(telecom$international.plan,telecom$churn)
scatterplot(telecom$voice.mail.plan,telecom$churn)
scatterplot(telecom$total.day.charge,telecom$churn)
scatterplot(telecom$total.day.calls,telecom$churn)
scatterplot(telecom$number.vmail.messages,telecom$churn)
summary(telecom)
library(corrplot)
x<-cor(telecom)
corrplot(x,method="circle")

#CREATING TRAIN AND TEST DATA
splitIndex<-createDataPartition(telecom$churn,p=0.70,list=FALSE,times = 1)
telecom_train<-telecom[splitIndex,]
telecom_test<-telecom[-splitIndex,]
table(telecom_train$churn)
table(telecom_test$churn)

#BUILDING LOGISTIC REGRESSION MODEL
fit<-glm(churn~.,data=telecom,family="binomial")
summary(fit)
step(fit)
fit1<- glm(formula = churn ~ international.plan + voice.mail.plan + 
             number.vmail.messages + total.day.charge + total.eve.charge + 
             total.night.charge + total.intl.calls + total.intl.charge + 
             customer.service.calls, family = "binomial", data = telecom)
summary(fit1)

#MODEL EVALUATION
pr<-predict(fit1,newdata =telecom_train,type="response" )
head(pr)
pr1<-ifelse(pr>0.5,1,0)
head(pr1)
confusionMatrix(as.factor(pr1),as.factor(telecom_train$churn),positive = "1")
pr2<-ifelse(pr>0.25,1,0)
confusionMatrix(as.factor(pr2),as.factor(telecom_train$churn),positive = "1")

pr4<-predict(fit1,newdata =telecom_test,type="response" )
pr4<-ifelse(pr4>0.25,1,0)
confusionMatrix(as.factor(pr4),as.factor(telecom_test$churn),positive = "1")

#MCFADDEN TEST
library(pscl)
pR2(fit1)

#CHECK AREA UNDER THE CURVE
library(InformationValue)
plotROC(actuals=telecom_train$churn,predictedScores = as.numeric(fitted(fit1)))





