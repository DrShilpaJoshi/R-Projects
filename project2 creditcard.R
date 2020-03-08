credit.default <- read.csv("~/Desktop/credit-default.csv", header=FALSE)
View(credit.default)
colnames(credit.default)
head(credit.default)
dim(credit.default)
df_credit=credit.default
summary(df_credit)
s=sample(nrow(df_credit),.75*nrow(df_credit))
s
df_tr_credit=df_credit[s,]
df_test_credir=df_credit[-s,]
install.packages("lmtest")
library(lmtest)
##### findout significant variablr and make ohter model####

logit_mod1=glm(default.payment.next.month~.,family="binomial",data=df_tr_credit)
summary(logit_mod1)

logit_mod2=glm(default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_3+BILL_AMT1+PAY_AMT1+PAY_AMT2+PAY_AMT5,family="binomial",data=df_tr_credit)
summary(logit_mod2)######ANOVA ###

#####likelyhoood ratio test###
logit_mod3=glm(default.payment.next.month~PAY_0+PAY_3+PAY_AMT1+PAY_AMT2+PAY_AMT5,family="binomial",data=df_tr_credit)
summary(logit_mod3)




library(lmtest)
lrtest(logit_mod1,logit_mod2)
####chi square is same as anova##
##lr test##
###p<...###
install.packages("pscl")
library(pscl)

pR2(logit_mod1)
pR2(logit_mod2)
pR2(logit_mod3)
###MCFADDED R2 value are more than 1indicating both the models have significant power###
#### WALD TEST--ATTRIBUTR POWER TEST###
###DO regtermtest for all signip1=predict(logit_mod1,df_test_credir,type="response")

### Prediction###
p1=predict(logit_mod1,df_test_credir,type="response")
p1
p2=predict(logit_mod2,df_test_credir,type="response")
p2
p3=predict(logit_mod3,df_test_credir,type="response")
p3

predict_pf1=ifelse(p1>.6,1,0)
predict_pf2=ifelse(p2>.7,1,0)
predict_pf3=ifelse(p3>.7,1,0)

predict_pf1
predict_pf2
predict_pf2
accuracy1=table(df_test_credir$default.payment.next.month,predict_pf1)
accuracy1
accuracy2=table(df_test_credir$default.payment.next.month,predict_pf2)
accuracy2=table(df_test_credir$default.payment.next.month,predict_pf2)

accuracy2
accuracy3=table(df_test_credir$default.payment.next.month,predict_pf3)
accuracy3
sum(diag(accuracy1))/sum(accuracy1)
sum(diag(accuracy2))/sum(accuracy2)
sum(diag(accuracy3))/sum(accuracy3)
ficant variblae###

install.packages("survey")
library(survey)
regTermTest(logit_mod2,"PAY_AMT5")
regTermTest(logit_mod2,"PAY_AMT1")
regTermTest(logit_mod2,"PAY_AMT2")
regTermTest(logit_mod2,"EDUCATION")
##### ALL ARE BELOW 0 SO NO USE OF THIS TEST##install.packages("caret")
library(caret)
varImp(logit_mod1)
#### PAY-0 has maximum VAlue###
varImp(logit_mod2)
###Pay 0 has maximum value##
varImp(logit_mod1)
varImp(logit_mod3)

ggplot(df_tr_credit,aes(PAY_0,logit_mod3$fitted.values))+geom_point()+geom_smooth()
summary(logit_mod3)
######  Prepare Model -4 after finding out PAY-0 has maximum value####
logit_mod4=glm(default.payment.next.month~PAY_0,family="binomial",data=df_tr_credit)
summary(logit_mod4)
p4=predict(logit_mod4,df_test_credir,type="response")
p4

predict_pf5=ifelse(p4>.15,1,0)
predict_pf5
accuracy5=table(df_test_credir$default.payment.next.month,predict_pf5)
accuracy5
sum(diag(accuracy5))/sum(accuracy5)
library(ggplot2)
ggplot(df_tr_credit,aes(PAY_0,logit_mod4$fitted.values))+geom_point()+geom_smooth()
#####Receiving operative characteristics(ROC)CURVE###
library(pROC)
roc(default.payment.next.month~logit_mod1$fitted.values,plot=TRUE,data = df_tr_credit)
roc(default.payment.next.month~logit_mod2$fitted.values,plot=TRUE,data = df_tr_credit)
roc(default.payment.next.month~logit_mod3$fitted.values,plot=TRUE,data = df_tr_credit)
roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit)
roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit)
roc(default.payment.next.month~logit_mod5$fitted.values,plot=TRUE,data = df_tr_credit,legacy.axes=TRUE,xlab="falsepositive %",ylab="True Positive %")
### CHECKING THRESHOLD####
roc_df=roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit,legacy.axes=TRUE,xlab="falsepositive %",ylab="True Positive %")

head(roc_df)
roc_data=roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit)
roc_data=roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit,legacy.axes=TRUE,xlab="falsepositive %",ylab="True Positive %")

roc_df=data.frame(cbind("true+ve"=roc_data$sensitivities,"false +ve"=roc_data$specificities,thresholds=roc_data$thresholds))
head(roc_df)
d_credit=subset(roc_df,roc_df[,1]>=80 &roc_df[,1]<=20,select=c(thresholds))
d_credit
nrow(roc_df)
nrow(roc_data)
length(roc_data)
summary(logit_mod4)
logit_mod4=glm(default.payment.next.month~PAY_0,family="binomial",data=df_tr_credit)
nrow(df_tr_credit)
logit_mod4$fitted.values
length(logit_mod4$fitted.values)
roc_data=roc(default.payment.next.month~logit_mod4$fitted.values,plot=TRUE,data = df_tr_credit,legacy.axes=TRUE,xlab="falsepositive %",ylab="True Positive %")
length(roc_df)
roc_df=data.frame(cbind("true+ve"=roc_data$sensitivities,"false +ve"=roc_data$specificities,thresholds=roc_data$thresholds))
nrow(roc_df)
roc_df
d_credit=subset(roc_df,roc_df[,1]>=80 &roc_df[,1]<=20,select=c(thresholds))
d_credit
d_credit=subset(roc_df,roc_df[,1]>=80 &roc_df[,2]<=20,select=c(thresholds))
d_credit
d_credit=subset(roc_df,roc_df[,1]>=.80 &roc_df[,2]<=.20,select=c(thresholds))
> d_credit
d_credit=subset(roc_df,roc_df[,1]>=.80 &roc_df[,2]<=.20,select=c(thresholds))
d_credit
nrow(d_credit)





