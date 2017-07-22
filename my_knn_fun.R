rowd <- sample(1:nrow(iris),round(nrow(iris)*0.8))
trainset <-  iris[-rowd,]
testset <-  iris[rowd,]

model <- knn(trainset[,-5], testset[,-5], trainset$Species, k = 17)
table(testset[,5],model)
sum(diag(table(testset[,5],model)))/nrow(testset)

knn.cv(trainset[,-5],  trainset$Species, k = 3, prob = TRUE)

my_knn_fun <- function(train,test,classvar){
	keep_vars <- names(train)[!(names(train) %in% classvar)]
	final <- data.frame()
	for(i in 1:round(nrow(train)/10)){
		model <- 
		knn(train[,keep_vars], test[,keep_vars], train[,classvar], k = i)
		accy <- sum(diag(table(test[,5],model)))/nrow(test)
		accy_data <- data.frame(iter= i, accuracy = accy)
		if(i == 1){
			final <- accy_data
		}else{
			final <- rbind(final,accy_data)
		}
	}
	
	return(final)
}
my_knn_fun(trainset, testset, "Species")
