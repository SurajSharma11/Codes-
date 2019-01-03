### Association Rule mining in R - Demo 

## ARules on Unsupervised data set

# Installing packages
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

#.	Read 'Transactions.csv' data into R such that the arules package treats the input csv file as "transaction" data.
trans = read.transactions(file="Transactions.csv", rm.duplicates= FALSE,
                          format="single",sep=",",cols =c(1,2))

#.	Explore and understand the data and items of transaction data
inspect(trans)
trans
image(trans)
trans@itemInfo

itemFrequency(trans)

#. Frequent itemsets
itemFrequencyPlot(trans)


#. Rule inducion
rules <- apriori(trans,parameter = list(sup = 0.1, conf = 0.1,target="rules"))
rules

summary(rules)
inspect(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))

#. Subset Rules
top_rules = sort(rules, by = c("confidence", "support"))
head(as(top_rules, "data.frame"), n=5)

rules.itemfilter1 <- as(subset(rules, subset = rhs %in%  "Choclates"), "data.frame")
rules.itemfilter1 
rules.itemfilter2 <- as(subset(rules, subset = lhs %in% "Pencil"),  "data.frame")
rules.itemfilter2 

rules_Lift <- as(subset(rules, subset = rhs %in% "Pencil" & lift > 1.01),"data.frame")
rules_Lift



#Market Basket Analysis with R library data:
data("Groceries")
subset1=Groceries[1:50]
subset1
subset2 <- subset1[,itemFrequency(subset1)>0.1]
subset2
itemFrequency(subset2)
itemFrequencyPlot(subset2)
rules <- apriori(subset2,parameter = list(sup = 0.05, conf = 0.5,target="rules"))
summary(rules)
inspect(rules)
rules <- apriori(subset2,parameter = list(sup = 0.005, conf = 0.5,target="rules"))
summary(rules)
inspect(rules)
image(subset2)





## ARules on Supervised data set

rm(list=ls(all=T))
library(arules)
library(arulesViz)

# Read in the titanic survival data set and see if its a categorical dataset
titanic_data <- read.csv(file = "titanic_data.csv")
head(titanic_data,10)
str(titanic_data)

# Apply Apriori on the data to find associations amongst all the attributes
rules <- apriori(titanic_data)
inspect(rules)

rules <- apriori(titanic_data,parameter = list(minlen=2, supp=0.005, conf=0.7),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"))

# Sort the rules based on "lift"
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# Visualizing the pruned rules
library(arulesViz)
plot(rules.sorted, method="graph", control=list(type="items"))

