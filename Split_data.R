## R Code for sampling train data and test data
split_data <- function(dataIn, split_wind=0.8){
  k <- round(0.8 *nrow(dataIn))
  rowID <- sample(1:nrow(dataIn),k)
  train_data <- dataIn[rowID,]
  test_data <- dataIn[-rowID,]
  return(list(train_data=train_data, test_data=test_data))
}
