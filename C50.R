rm(list=ls(all=TRUE))
#Setting working directory

#Reading the data
data=read.table('data.csv', 
                header=T,sep=',',
                col.names=c('ID','age','exp','inc','zip',
                            'family','ccavg','edu','mortgage',
                            'loan','securities','cd','online',
                            'cc'))


#Dropping the not required attributes (ID,Zip)
names(data)
data <- data[,-c(1,5)]
names(data)

#Converting the attributes to appropriate data types
cols <- c("family","edu","securities","cd","online","cc","loan")
data[cols] <- data.frame(apply(data[cols],2,as.factor))
str(data)

#We can also use CreateDataPartition function from caret package that does stratified sampling
library(caret)
x<-createDataPartition(data$loan,times=1,p=0.7,list=F)
train=data[x,]
test=data[-x,]


#Decision Trees using C5.0 (For Classification Problem)
#Loading library for C5.0
library(C50)

#calling C5.0 function
dtC50 <-  C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

#Model Evaluation- Error Metrics 
cm_train <-  table(train$loan, predict(dtC50, newdata=train, type="class"))
sum(diag(cm_train))/sum(cm_train)

cm_test <- table(test$loan, predict(dtC50, newdata=test, type="class"))
sum(diag(cm_test))/sum(cm_test)

