rm(list=ls(all=TRUE))

## Read Data
bank <- read.csv("UniversalBank.csv")

# Understanding the data
summary(bank)
str(bank)
head(bank)

# Removing unnecessary columns ID and zipcode
bank <- subset(bank, select=-c(ID,ZIP.Code))

# Do necessary type conversions
cat_col <- c("Family","Education","Personal.Loan","Securities.Account","CD.Account",
             "Online","CreditCard")

bank[cat_col] <- lapply(bank[cat_col],factor)

# Do Train-Test Split
library(caret)
set.seed(1)
rows <- createDataPartition(bank$Personal.Loan,p = 0.7,list = FALSE)
train <- bank[rows,]
test <- bank[-rows,]

# PreProcess the data to standadize the numeric attributes
num_cols <- setdiff(colnames(train),cat_col)
preProc <- preProcess(train[,num_cols],method = c("center", "scale"))
train <- predict(preProc,train)
test <- predict(preProc,test)

## create dummies for factor variables
train_dummy <- data.frame(model.matrix(~.,train)[,-1])
test_dummy <- data.frame(model.matrix(~.,test)[,-1])

## Seperate X and y
x_train <- train_dummy[,-11]
y_train <- train_dummy$Personal.Loan1
x_test <- test_dummy[,-11]
y_test <- test_dummy$Personal.Loan1

## Building model on SVC

library(e1071)

svc = svm(x_train,y_train, type = "C-classification", kernel = "linear",cost = 10)
summary(svc)

pred_train = predict(svc,x_train)
pred_test = predict(svc,x_test)

cm = table(pred_test,y_test)
acc = sum(diag(cm))/sum(cm)
acc


## Parameter Tuning 

trctrl = trainControl(method = "cv",number = 3)
grid = expand.grid(cost = c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5,10))

set.seed(1)

vm_Linear_Grid = caret::train(x_train,factor(y_train),method = "svmLinear2",trControl = trctrl,tuneGrid = grid
                              ,class.weights = c("0"=1,"1"=10))
