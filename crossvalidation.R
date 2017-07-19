rowd <- sample(1:nrow(creditdat),round(nrow(creditdat)*0.2))
trainset <-  creditdat[-rowd,]
testset <-  creditdat[rowd,]
rowid<-  sample(rep(1:10,40),400)
final_data <- data.frame()

for(i in 1:10){
 trainset <-  creditdat[which(rowid != i),]
 testset <-  creditdat[which(rowid == i),]
 model <- lm(Limit ~Cards +Balance,data=trainset)
 # 1] "X"         "Income"    "Limit"     "Rating"    "Cards"     "Age"      
   #[7] "Education" "Gender"    "Student"   "Married"   "Ethnicity" "Balance"  

 model1 <- lm(Limit ~Rating +Balance,data=trainset)
 predict(model,testset)
 MSE1 <- (testset$Limit -predict(model,testset))^2
 MSE2 <- (testset$Limit -predict(model1,testset))^2
 finaldata <- data.frame(iteration= i, MSE1 = mean(sqrt(MSE1)),MSE2 = mean(sqrt(MSE2)))
 if(nrow(final_data)==0){
  final_data= finaldata 
  }else{
  final_data= rbind(final_data,finaldata)
 }
}
