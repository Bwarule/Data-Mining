## Read the data file 
modeling_data <- read.csv("C:\\Papers\\training\\shar\\4\\ds1.10.csv")
## we will assume missing value impuatation is done.
## outlier cappin is done.

## split the into the train and test data
set.seed(1)
dev_sample <- 0.8

x3 <- nrow(modeling_data)
x2 <- as.integer(x3*(dev_sample))
sub <- c(sample(1:x3, x2))
	
train_sample <- modeling_data[sub,]
test_sample <- modeling_data[-sub,]	

## Run the Data_redconstvar code for further analysis
source("C:\\Papers\\training\\shar\\4\\Data_redconstvar.R")
modeling_data$uniq_var <- "IND"
modeling_data$income <- 0

output <- 
Datareduc_Subroutine_withConstvar (train_sample, 
constVAR = TRUE, max_cat = 10, idvars = NULL, RemovconstVAR = TRUE) 

## use of library FSelector for varies measure 
library(FSelector)

weights <- information.gain(target~., train_sample)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "target")
print(f)

weights <- gain.ratio(target~., train_sample)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "target")
print(f)

weights <- symmetrical.uncertainty(target~., train_sample)
print(weights)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "target")
print(f)

## Variable selection process
library(leaps)
library(MASS)
model <- 
regsubsets(target ~  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10  ,  data = train_sample, nvmax = 4)

model <- 
regsubsets(target ~  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10  ,  data = train_sample, method = "backward")

## ("exhaustive","backward", "forward", "seqrep"
model <- 
regsubsets(target ~  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10  ,  data = train_sample, method = "backward")
library(Hmisc)

lr <- glm(target~x3,data=train_sample,family = binomial)

predit <- fitted.values(lr);
n <- length(train_sample$target);
target <- train_sample$target
## target<-rep(0,times=n);
## for (i in 1:n) {if (treedata$y[i]=='yes') target[i]=1};

iv <- function(predit,target) # my somer's D function
{
 data<-data.frame(predit,target);
 data_sort<-data[order(predit),]

 ttl_num<-length(target);
 bin<-10;
 n<-ttl_num%/%bin;
 iv_bin<-rep(0,times=bin);
 good<-rep(0,times=bin);
 bad<-rep(0,times=bin);
 for (i in 1:bin) # calculate PSI for ith bin
 {
 if(i!=bin) {good[i]<-sum(data_sort$target[((i-1)*n+1):(n*i)]);bad[i]<-n-good[i]} else
 {good[i]<-sum(data_sort$target[((i-1)*n+1):ttl_num]);bad[i]<-ttl_num-n*(i-1)-good[i]}
 }

 good_pct<-good/sum(good)
 bad_pct<-bad/sum(bad)
 for (i in 1:bin)
 {
 iv_bin[i]<-(bad_pct[i]-good_pct[i])*log(bad_pct[i]/good_pct[i])
 }

 iv=sum(iv_bin)
 return (iv)
}

iv(predit,target)


## Fit the logistic 
library(forward)
input_vars <- c("x2","x3","x4")
formulas <- paste("target", sep="~",
	fwd.combn(input_vars, length(input_vars), fun=function(x){paste(x,collapse="+")}))

Ffit_LR <- glm(eval(parse(text=formulas)),
			data=train_sample,family=binomial(link="logit")) 
			
## Model validation
library(ROCR)
library(doBy)
library(gdata)
train_sample$LR_pred <- predict(Ffit_LR, type='response', train_sample)
test_sample$LR_pred <- predict(Ffit_LR, type='response', test_sample)
	
    test_sample_out <- test_sample

    test_sample_out$predicted_val <- 
	predict(Ffit_LR, type='response', test_sample)
	
	retain_vars_out <- c("target","predicted_val")
	
	test_sample_out <- test_sample_out[retain_vars_out]

    ## Adding Model Code ## 
    test_sample_out$model_code <- "cs" 

    ## Decile ## 
    test_sample_out$predicted_val <- 
    as.numeric(test_sample_out$predicted_val)
    test_sample_out <- 
    test_sample_out[order(-test_sample_out$predicted_val),]
    test_sample_out$rank <- rep(1:dim(test_sample_out)[1],each=1)
    test_sample_out$decile1 <- 
    test_sample_out$rank/dim(test_sample_out)[1] * 10
    test_sample_out$decile <- floor(test_sample_out$decile1)
    test_sample_out$decile[test_sample_out$decile > 9] <- 9
    test_sample_out$decile1 <- NULL
    test_sample_out$rank <- NULL
    test_sample_out$model_code <- as.factor(test_sample_out$model_code)

    myfun1 <- function(x){
		c(mean=mean(x, na.rm=TRUE))
	}
	
	attach(test_sample_out)
    summaryBy_Decile <-
	summaryBy(predicted_val ~decile, data=test_sample_out,
	FUN=myfun1,keep.names=TRUE)
    detach(test_sample_out)

    names(summaryBy_Decile)[names(summaryBy_Decile)=="predicted_val"] <-
    "avg_decileby"	
    ##summaryBy_Decile$mdl_cde <- args_2
	
	## COMPUTING ROC CURVE (x-axis: fpr, y-axis: tpr)
	
	pred_LR1 <- prediction(train_sample$LR_pred, train_sample[,"target"])
    perf_LR11 <- performance(pred_LR1,"tpr","fpr")

    pred_LR2 <- prediction(test_sample$LR_pred, test_sample[,"target"])
    perf_LR21 <- performance(pred_LR2,"tpr","fpr")
	
	## PRECISION/RECALL CURVE (x-axis: recall, y-axis: precision)
	perf_LR12 <- performance(pred_LR1, "prec", "rec")
	perf_LR22 <- performance(pred_LR2, "prec", "rec")
	
	## SENSITIVITY/SPECIFICITY CURVE
	## (x-axis: specificity, y-axis: sensitivity)
	perf_LR13 <- performance(pred_LR1, "sens", "spec")
	perf_LR23 <- performance(pred_LR2, "sens", "spec")
	
	## LIFT CHART
	perf_LR14  <- performance(pred_LR1,"lift","rpp")
	perf_LR24  <- performance(pred_LR2,"lift","rpp")
	
	## CALCULATING KS STATISTICS 
	KS_LR1 <- 
	max(attr(perf_LR11,'y.values')[[1]]-attr(perf_LR11,'x.values')[[1]])
	KS_LR2 <- 
	max(attr(perf_LR21,'y.values')[[1]]-attr(perf_LR21,'x.values')[[1]])
	
	## CALCULATING AREA UNDER ROC & Gini(Accuracy Ratio)
	ROC_LR1 <- attr(performance(pred_LR1,"auc"),'y.values')
	ROC_LR2 <- attr(performance(pred_LR2,"auc"),'y.values')
	ROC_LR1 <- ROC_LR1[[1]]
	ROC_LR2 <- ROC_LR2[[1]]

    Gini_LR1 <-(ROC_LR1-0.5)*2
    Gini_LR2 <-(ROC_LR2-0.5)*2
	
	## LR MODEL SUMMARY
    cexmain <- 1.2
    
    #pdf(file= file.path(op_dir_path,
	#paste(model_version_string ,"Result.pdf", sep="_" )))
	
	textplot(c("Logistic Regression Result",
	    paste("Date:",as.character(Sys.Date()))),
		col='purple',cex=cexmain,
		valign="top")	
		
    textplot(capture.output(summary(Ffit_LR )),valign="top")
    title(main = list("Parameter Estimates", cex=cexmain,col="purple", font=3))
      
	textplot(capture.output(t(t(data.frame(VIF=vif(Ffit_LR))))),
	valign="top",halign="center",fixed.width=TRUE,cex= 0.5)
	title(main = list("Collinearity diagnostics:VIF", 
	cex=cexmain, col="purple", font=3))
	  
	plot(perf_LR11,col='blue',lty=1);
	plot(perf_LR21, col='green',add=TRUE,lty=2);
	legend("bottomright",  c('Train','Test'),inset=.05,
		fill=c('blue','green'),horiz=FALSE)
	title(main = list("ROC Curve",
 		cex=cexmain, col="purple", font=3))
	
	plot(perf_LR12,col='blue',lty=1);
	plot(perf_LR22, col='green',add=TRUE,lty=2);
	legend("topright",  c('Train','Test'),inset=.05,
		fill=c('blue','green'),horiz=FALSE)
	title(main = list("Precision/Recall Curve", 
		cex=cexmain, col="purple", font=3))
	
	plot(perf_LR13,col='blue',lty=1);
	plot(perf_LR23, col='green',add=TRUE,lty=2);
	legend("topright",  c('Train','Test'),inset=.05,
		fill=c('blue','green'),horiz=FALSE)
	title(main = list("Sensitivity/Specificity",
		cex=cexmain, col="purple", font=3))
	
	plot(perf_LR14,col='blue',lty=1);
	plot(perf_LR24, col='green',add=TRUE,lty=2);
	legend("topright",  c('Train','Test'),inset=.05,
		fill=c('blue','green'),horiz=FALSE)
	title(main = list("Lift Chart", cex=cexmain ,col="purple", font=3))
		
	Outtime_chart <- file.path(param_path_code,"Integrated_Outtime_chart.Rc")
	loadcmp(file=Outtime_chart, envir = .GlobalEnv, chdir = FALSE)
		
	DataOut <- 
	Outtimechart(test_sample_out, plotTrue=TRUE,outputpathGraph=op_dir_path)
	
    ##.......... Coefficient Plot for Logistic Regression ............###
    FfitData <- summary(Ffit_LR)
	coefficients.Data <- FfitData$coefficients	
	colinsert <-  c(as.numeric(coefficients.Data[,"Pr(>|z|)"])) 
	colinsert[colinsert < 0.05] <- 'green' 
	colinsert[colinsert!='green'] <- 'red' 
	colinsert <- colinsert[-1] ##..remove of Intercept		 
	
	coefplot(Ffit_LR,cex.var=0.65, cex.pts=1.6,
		mar=c(1,14,5.1,2), col.pts=colinsert, col='blue')	
	legend("bottomright", c('Significant variables','UnSignificant variables'),
	 	fill=c("green","red"), horiz=FALSE, cex=0.55)

	dev.off() 
	
	## GET MODEL SIGNIFICANT VARIABLES
	LR_mdl_sig_vars <- all.vars(as.formula(Ffit_LR))
	del_list <- list(LR_mdl_sig_vars[1])
    LR_mdl_sig_vars <- LR_mdl_sig_vars [!LR_mdl_sig_vars %in% del_list] 
	Sig_vars <- 
	data.frame(model_type="LR",significant_variable_name=LR_mdl_sig_vars)
	assign("Sig_vars", Sig_vars, envir = .GlobalEnv)
	
	assign("ROC_LR1", ROC_LR1, envir = .GlobalEnv)
	assign("ROC_LR2", ROC_LR2, envir = .GlobalEnv)
	assign("Gini_LR1", Gini_LR1, envir = .GlobalEnv)
	assign("Gini_LR2", Gini_LR2, envir = .GlobalEnv)
	assign("KS_LR1", KS_LR1, envir = .GlobalEnv)
	assign("KS_LR2", KS_LR2, envir = .GlobalEnv)
	
	## SAVE MODEL TO REPOSITORY
	save(Ffit_LR, 
	file = file.path(mdl_repo,paste(model_version_string,"RData", sep=".")))
		
	write.csv(test_sample_out , file = file.path(op_dir_path,
    gsub(" ","",paste(model_version_string,".csv"))),row.names = FALSE)

    Dname_out <- paste(model_version_string,"Decile",sep="_")
	
    write.csv(summaryBy_Decile, file = file.path(mdl_result_path,
    gsub(" ","",paste(Dname_out,".csv"))),row.names = FALSE)
			
