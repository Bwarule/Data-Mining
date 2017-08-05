## Tree model in R
## via rpart, tree, caret, C50, etc.
## https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
## https://rpubs.com/RatherBit/78793
## https://www.r-bloggers.com/logistic-regression-in-r-part-one/
## https://www.r-bloggers.com/evaluating-logistic-regression-models/
## https://www.analyticsvidhya.com/blog/2015/09/complete-guide-boosting-methods/
## http://statweb.stanford.edu/~jhf/ftp/stobst.pdf
## very good refrence
## http://www.ccs.neu.edu/home/vip/teach/MLcourse/4_boosting/slides/gradient_boosting.pdf

setwd("D:\\UCL")
# UCL_data <- read.table("abalone.txt",sep=",",header = FALSE)
adultData <- read.csv2("http://www.insular.it/?wpdmact=process&did=OC5ob3RsaW5r",
 header = TRUE, strip.white = TRUE)

## Get the basic information 
dim(adultData)
head(adultData)
str(adultData)
summary(adultData)
sapply(adultData,class)

plot(adultData, col = pal, ylab=expression(italic("Salary\n")),xlab="", main=expression(italic("Marital status")), cex=0.8)

## ID3 tree model
library(data.tree) ### ID3 
library(party)
library(rpart) ## CART
library(randomForest) 
library(fastAdaboost)
library(C50)
library(adabag) ## ... bagging is present in both library
library(ipred)
library(caret)      # same as before, this does not work in knitr, unless you have installed the package in RStudio

fakedata <- data.frame( X=c(rnorm(100,0,1),rnorm(100,1,1)), Y=c(rep(0,100),rep(1,100) ) )
fakedata$Y <- factor(fakedata$Y)
test_adaboost <- adaboost(Y~X, data=fakedata,10)

selected <- c("age", "education", "marital.status", "relationship", "sex", "hours.per.week", "salary")
adultData <- subset(adultData, select = selected)
	
trainIndex = createDataPartition(adultData$salary, p=0.60, list=FALSE)
training = adultData[ trainIndex, ]
test = adultData[ -trainIndex, ]

## The model is based on the recursive partitioning for classification, regression and survival trees (rpart).
## See here for details: 
  
set.seed(33833) # for reproducibility
modFit <- train(salary ~ ., method = "rpart", data=training)
fit <- rpart(salary ~ ., data=training)
## modFitglm <- train(salary ~ ., method="glm", family="binomial", data=training)
## exp(coef(modFitglm$finalModel))

## modFitpls <- train(salary ~ ., method = "pls", data=training) --- pls discriminate analysis  
## modFitgbm <- train(salary ~ ., method = "gbm", data=training) --- gbm workes with good accurcy 
## modFitC5.0 <- train(salary ~ ., method = "C5.0", data=training)  ## C5.0 is not working
prediction_A <- predict(fit, newdata=training,type="class")
tab <- table(prediction_A, training$salary)
sum(diag(tab))/sum(tab)

prediction <- predict(fit, newdata=test,type="class")
tab <- table(prediction, test$salary)
sum(diag(tab))/sum(tab)


prediction <- predict(modFit, newdata=test)
tab <- table(prediction, test$salary)
sum(diag(tab))/sum(tab)

prediction <- predict(modFitgbm, newdata=test)
tab <- table(prediction, test$salary)
sum(diag(tab))/sum(tab)

rpartPred<-predict(modFit,test)
confusionMatrix(rpartPred,test$salary) 

rpartPred<-predict(modFitgbm,test)
confusionMatrix(rpartPred,test$salary) 

library(htmlTable)
# htmlTable( ) ## Good output style
mytableout = htmlTable (confusionMatrix(rpartPred,test$salary))
print(mytableout,type="html",useViewer=TRUE)

outdir.tables = getwd() # Any directory
setwd(outdir.tables)
sink("mytable.html")
print(mytableout,type="html",useViewer=TRUE)
sink()

library(R2HTML)
 HTMLStart(outdir=getwd(), file="myreport",
    extension="html", echo=FALSE, HTMLframe=TRUE)
 HTML.title("My Report", HR=1)

 HTML.title("Description of my data", HR=3)
 print(confusionMatrix(rpartPred,test$salary)) 

 HTMLhr()

 HTMLStop() 
 
InformationGain &lt;- function( tble ) {
  tble &lt;- as.data.frame.matrix(tble)
  entropyBefore &lt;- Entropy(colSums(tble))
  s &lt;- rowSums(tble)
  entropyAfter &lt;- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain &lt;- entropyBefore - entropyAfter
  return (informationGain)
}

## C4.5 tree model
churnNew <- factor(churnTrain$churn,levels =c("no","yes"))
churnTrain$churn_flag <- "0"
churnTrain$churn_flag[which(churnTrain$churn == "no")] <- "1"
churnTrain$churn_flag <- as.factor(churnTrain$churn_flag)
treeModel <- C5.0(x = churnTrain[, c(-20,-21)], y = churnTrain$churn)

## CART model

## CHAID model in R

## fastAdaboost
library(fastAdaboost)

fakedata <- data.frame( X=c(rnorm(100,0,1),rnorm(100,1,1)), Y=c(rep(0,100),rep(1,100) ) )
fakedata$Y <- factor(fakedata$Y)
test_adaboost <- adaboost(Y~X, data=fakedata,10)

## Bagging model in R
library("MASS")
library("survival")

# Classification: Breast Cancer data

data("BreastCancer", package = "mlbench")

# Test set error bagging (nbagg = 50): 3.7% (Breiman, 1998, Table 5)
library(adabag) ## ... bagging is present in both library
library(ipred)
mod <- bagging(Class ~ Cl.thickness + Cell.size
                + Cell.shape + Marg.adhesion   
                + Epith.c.size + Bare.nuclei   
                + Bl.cromatin + Normal.nucleoli
                + Mitoses, data=BreastCancer, coob=TRUE)
