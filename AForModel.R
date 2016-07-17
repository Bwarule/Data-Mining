
	AForModel <- function(model,Resp,train_sample,test_sample){
		table(train_sample[,Resp],round(predict(model, type='response', train_sample)))
		table(train_sample[,Resp])
		table(test_sample[,Resp],round(predict(model, type='response', test_sample)))	
		table(test_sample[,Resp])
		A_train <- sum(diag(table(train_sample[,Resp],round(predict(model, type='response', train_sample)))))/nrow(train_sample)
		A_test <- sum(diag(table(test_sample[,Resp],round(predict(model, type='response', test_sample)))))/nrow(test_sample)
		return(list(A_train,A_test))
	}
