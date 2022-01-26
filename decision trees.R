suppressMessages(library('knitr'))
suppressMessages(library('dplyr'))
suppressMessages(library('tidyverse'))
suppressMessages(library(datarium))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(readr))
suppressMessages(library(party))
suppressMessages(library(ggplot2))
##setwd("F:/3rd Year/3505_Foundation of Data Analytics/Project/decision tree")
data1 <- read.csv("zz.csv")
View(data1)

rm(list=ls())

#### PREPARING THE DATASET #####
str(data1)
dim(data1)
summary(data1)

#### PARTITIONING DATASET INTO VALIDATION DATASETS ####

data1$Aggregate.ratingF <- factor(data1$Aggregate.rating)
set.seed(1234)
pd <- sample(2,nrow(data1),replace=TRUE, prob=c(0.8,0.2))
train <- data1[pd==1,]
validate <- data1[pd==2,]


#### DECISION TREE WITH PARTY ####
View(data1)
#setting the confidence level
# setting minimum size as 200 => branch will split into 2 only when sample size is 200

tree <- ctree(Aggregate.ratingF~Votes,data=train, controls = ctree_control(mincriterion = 0.9, minsplit=200))
tree
plot(tree)

tree1 <- ctree(Aggregate.rating~Votes,data=train)
tree1
dev.off()
plot(tree1)
## when there are restaurants with average cost for 2 people within set price range and with votes
tree2 <- ctree(Aggregate.ratingF~Price.range+Votes,data=train)
tree2
dev.off()
plot(tree2)

### PREDICTION ###
predict(tree,validate,type="prob")
predict(tree1,validate,type="prob")
predict(tree2,validate,type="prob")


#### USING RPART LIBRARY ####
### creating a new tree ###
tree3 <- rpart(Aggregate.ratingF~Votes,train)
rpart.plot(tree3,extra = 2)
newtree2 <- rpart(Aggregate.rating~Average.Cost.for.two+Price.range+Votes,data=train)
newtree2
rpart.plot(newtree2)