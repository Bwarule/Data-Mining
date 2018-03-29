setwd("C:\\Users\\bharat.warule\\Desktop\\German_Credit")

list.files()

data1 <- read.csv("German_Credit2.csv")
#  "German_Credit2.xlsx" 
data2 <- read.csv("German_Credit21.csv")
## data 2 don't have missing value
summary(data1)
## having missing value 
count_missing <- function(x){return(sum(is.na(x)))}
sapply(data1,count_missing)
newdat <- sapply(data1,count_missing)

## imput the missing value 
vars_name <- names(data1)
keyvars <- c("OBS","RESPONSE" )

used_vars <- vars_name[!(vars_name %in% keyvars)]
for(j in 1:length(used_vars)){
	my_var_imput <- used_vars[j] 
	getmean <- mean(data1[,my_var_imput],na.rm=TRUE)
	data1[is.na(data1[,my_var_imput]),my_var_imput] <- getmean

}

## data1 don't have missing value , all mssing value imputed by mean 
## Response variable # 3 missing vale 

data1new <- data1[!(is.na(data1$RESPONSE)),]

## new data having 997 rows

## Lets merge the files 
final_data <- merge(data1new, data2, by = "OBS")
dim(final_data)
[1] 997  29
## final data contains 997 rows and 29 columns 

## data expolration 

table(final_data$HISTORY,final_data$RESPONSE)
   
      0   1
  0  25  15
  1  28  21
  2 169 360
  3  27  60
  4  50 242
> 

# 0: no credits taken                                                                                                                   
# 1: all credits at this bank paid back duly 
# 2: existing credits paid back duly till now 
# 3: delay in paying off in the past  
# 4: critical account  

## Average balance in savings account
#
table(final_data$SAV_ACCT,final_data$RESPONSE)
## <100DM is more dengers for event 
      0   1
  0 216 384
  1  34  69
  2  11  52
  3   6  42
  4  32 151


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
final_data$RESPONSE1 <- 1- final_data$RESPONSE
final_data$RESPONSE <- NULL

final_data$RESPONSE1 <- as.factor(final_data$RESPONSE1)
trainIndex = createDataPartition(final_data$RESPONSE1, p=0.80, list=FALSE)
training = final_data[ trainIndex, ]
test = final_data[ -trainIndex, ]

## Lets fit the Tree model
set.seed(33833) # for reproducibility
modFit <- train(RESPONSE1 ~ ., method = "rpart", data=training)

## Do the predicton on test set
prediction <- predict(modFit, newdata=test)
tab <- table(prediction, test$RESPONSE1)
sum(diag(tab))/sum(tab)
## Accuracy 0.71
## sensitivity 0.559322

## lets Try the gbm model , which used multiple decision tree 
modFitgbm <- train(RESPONSE1 ~ ., method = "gbm", data=training) ## --- gbm workes with good accurcy 
prediction <- predict(modFitgbm, newdata=test)
tab <- table(prediction, test$RESPONSE1)
sum(diag(tab))/sum(tab)
## accuracy 0.77
## sensitivity 0.5084


library(FSelector) 
weights <- information.gain(RESPONSE1~., training) 
print(weights) 
subset <- cutoff.k(weights, 3) 
f <- as.simple.formula(subset, "target") 
print(f) 

##
Variables	attr_importance
CHK_ACCT	0.05641526
DURATION	0.02086365
HISTORY	0.01937368
AMOUNT	0.01616008
SAV_ACCT	0.0155866
AGE	0.01225686

modFitglm <- glm("RESPONSE1~ CHK_ACCT + DURATION + HISTORY + log_AMOUNT + SAV_ACCT + AGE", family="binomial", data=training)
training$log_AMOUNT <- log(training$AMOUNT)
step_model <- step(modFitglm)

Call:
glm(formula = RESPONSE1 ~ CHK_ACCT + DURATION + HISTORY + SAV_ACCT + 
    AGE, family = "binomial", data = training)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8462  -0.7616  -0.4906   0.8992   2.5700  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.680138   0.377874   1.800 0.071875 .  
CHK_ACCT    -0.536414   0.074927  -7.159 8.12e-13 ***
DURATION     0.041645   0.007145   5.828 5.60e-09 ***
HISTORY     -0.344676   0.085088  -4.051 5.10e-05 ***
SAV_ACCT    -0.209885   0.061672  -3.403 0.000666 ***
AGE         -0.018708   0.008076  -2.316 0.020541 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 976.67  on 798  degrees of freedom
Residual deviance: 806.69  on 793  degrees of freedom
AIC: 818.69

Number of Fisher Scoring iterations: 4

exp(coef(step_model))

> exp(coef(step_model))
(Intercept)    CHK_ACCT    DURATION     HISTORY    SAV_ACCT         AGE 
  1.9741509   0.5848417   1.0425242   0.7084501   0.8106772   0.9814663 

prediction <- predict(step_model, newdata=test,type="response")
tab <- table(prediction >0.35, test$RESPONSE1)
sum(diag(tab))/sum(tab)
## accurcy 0.71
## sensitivity 0.34, at cut of 0.35(0.677)

## rank ordering 
 ## Decile ##  
 test_sample_out <- test
  test_sample_out$predicted_val <-  
     as.numeric(predict(step_model, newdata=test,type="response")) 
     test_sample_out <-  
    test_sample_out[order(-test_sample_out$predicted_val),] 
     test_sample_out$rank <- rep(1:dim(test_sample_out)[1],each=1) 
     test_sample_out$decile1 <-  
     test_sample_out$rank/dim(test_sample_out)[1] * 10 
     test_sample_out$decile <- floor(test_sample_out$decile1) 
    test_sample_out$decile[test_sample_out$decile > 9] <- 9 
     test_sample_out$decile1 <- NULL 
    test_sample_out$rank <- NULL 

 
     myfun1 <- function(x){ 
 		c(mean=mean(x, na.rm=TRUE)) 
 	} 
 	 
 	attach(test_sample_out) 
     summaryBy_Decile <- 
 	summaryBy(predicted_val ~decile, data=test_sample_out, 
	FUN=myfun1,keep.names=TRUE) 
     detach(test_sample_out) 
 
 
152     names(summaryBy_Decile)[names(summaryBy_Decile)=="predicted_val"] <- 
153     "avg_decileby"	 

   decile predicted_val
1       0    0.70084871
2       1    0.54890827
3       2    0.44082145
4       3    0.36889861
5       4    0.31224934
6       5    0.23781625
7       6    0.18234025
8       7    0.13276864
9       8    0.09329523
10      9    0.05205052


## modFitglm <- train(RESPONSE1 ~ ., method="glm", family="binomial", data=training)
## exp(coef(modFitglm$finalModel))

## modFitpls <- train(RESPONSE1 ~ ., method = "pls", data=training) --- pls discriminate analysis  
## modFitC5.0 <- train(RESPONSE1 ~ ., method = "C5.0", data=training)  ## C5.0 is not working
prediction_A <- predict(fit, newdata=training,type="class")
tab <- table(prediction_A, training$RESPONSE1)
sum(diag(tab))/sum(tab)

prediction <- predict(fit, newdata=test,type="class")
tab <- table(prediction, test$RESPONSE1)
sum(diag(tab))/sum(tab)

prediction <- predict(modFit, newdata=test)
tab <- table(prediction, test$RESPONSE1)
sum(diag(tab))/sum(tab)


## sample question at time of interivew 
## P Value : In statistical hypothesis testing, the p-value or probability value is the probability
#  for a given statistical model that, when the null hypothesis is true
