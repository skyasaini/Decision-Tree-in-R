##Classification Trees using rpart()##
library(dplyr)
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)


setwd('D:\\Data science\\Desion tree\\Dataset')
churn<-read.csv("churn.csv",header = TRUE)

summary(churn)

churn%>%filter(SeniorCitizen!=1 & SeniorCitizen!=0)%>%count()

churn$SeniorCitizen <- ifelse(churn$SeniorCitizen<=0.5,0,1)

summary(as.factor(churn$SeniorCitizen))

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

summary(churn$SeniorCitizen)

churn$tenure_group<-ifelse(churn$tenure>=0 & churn$tenure<=12,'0-1 Yr',
                           ifelse(churn$tenure>12 & churn$tenure<=24,'1-2 Yr',
                                  ifelse(churn$tenure>24 & churn$tenure<=48,'2-4 Yr',
                                         ifelse(churn$tenure>48 & churn$tenure<=60,'5 Yr','>5 Yr'))))
churn$tenure_group<-as.factor(churn$tenure_group)


#OnlineSecurity	OnlineBackup	DeviceProtectionService	TechnicalHelp	OnlineTV	OnlineMovies
summary(churn$OnlineSecurity)
churn$OnlineSecurity<-ifelse(churn$OnlineSecurity=="Yes","Yes","No")
churn$OnlineSecurity<-as.factor(churn$OnlineSecurity)

summary(churn$OnlineBackup)
churn$OnlineBackup<-ifelse(churn$OnlineBackup=="Yes","Yes","No")
churn$OnlineBackup<-as.factor(churn$OnlineBackup)

summary(churn$DeviceProtectionService)
churn$DeviceProtectionService<-ifelse(churn$DeviceProtectionService=="Yes","Yes","No")
churn$DeviceProtectionService<-as.factor(churn$DeviceProtectionService)

summary(churn$TechnicalHelp)
churn$TechnicalHelp<-ifelse(churn$TechnicalHelp=="Yes","Yes","No")
churn$TechnicalHelp<-as.factor(churn$TechnicalHelp)

summary(churn$OnlineTV)
churn$OnlineTV<-ifelse(churn$OnlineTV=="Yes","Yes","No")
churn$OnlineTV<-as.factor(churn$OnlineTV)

summary(churn$OnlineMovies)
churn$OnlineMovies<-ifelse(churn$OnlineMovies=="Yes","Yes","No")
churn$OnlineMovies<-as.factor(churn$OnlineMovies)


summary(churn$InternetConnection)
churn$InternetConnection<-ifelse(churn$InternetConnection=="Yes","Yes","No")
churn$InternetConnection<-as.factor(churn$InternetConnection)

colnames(churn)

churn<-churn[,-6]

cor(churn$TotalAmount,churn$MonthlyServiceCharges)

#spiliting data sets into test and training datasets
set.seed(2)
index<-sample(nrow(churn),.80*nrow(churn),replace = F)
train1<-churn[index,]
test1<-churn[-index,]

mod<-rpart(data=train1[,-1],Churn~.,control=rpart.control(cp=0.001,maxdepth=10),
           method="class",parms=list(split="gini"))

plot(mod, margin=0.1, main="Classification Tree")
text(mod, use.n=TRUE, all=TRUE, cex=.7)

fancyRpartPlot(mod)

printcp(mod)
plotcp(mod, minline = TRUE)


mod1<-prune(mod,cp= 0.002)

fancyRpartPlot(mod1)
#Rules derivation
mod1


test1$predicted<-predict(mod1,type = "class",newdata = test1)


confusionMatrix(test1$predicted,test1$Churn)

#kappa metric
kappa2(data.frame(churn$Churn,test1$predicted))

#ROC curve analysis
library(ROCR)
prediction(predictions = as.numeric(test1$predicted),labels = as.numeric(test1$Churn))->p
perf<-performance(p,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(p,"auc")
unlist(auc@y.values)


