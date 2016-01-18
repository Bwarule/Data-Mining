Datareduc_Subroutine_withConstvar <-
function(DataIn, constVAR=TRUE ,max_cat=100, idvars=NULL ,RemovconstVAR=TRUE){
	
	## 3.2 REMOVE VARIABLES WITH 
	## ZERO VARIANCE & CONSTANT TERMS
	vector_deL <- Info_onDel <- c()
	
	varNotnumeric <-
	names(DataIn)[which(lapply(DataIn,class)!='numeric')]
	varhasnumeric <-
	names(DataIn)[which(lapply(DataIn,class)=='numeric')]
	
	varNotnumeric <- varNotnumeric[!(varNotnumeric %in% idvars)]
	varhasnumeric <- varhasnumeric[!(varhasnumeric %in% idvars)]
	
	if(length(varNotnumeric)>= 1){
		for(i in 1: length(varNotnumeric)){
			x12 <- DataIn[,varNotnumeric[i]]
			
			if(length(unique(x12))==1){
				vector_deL <- c(vector_deL,varNotnumeric[i])
				reason <- 'category has unique value'
				Info_onDel <- c(Info_onDel, reason)
				}
			
			if(length(unique(x12))>= max_cat){
				vector_deL <- c(vector_deL,varNotnumeric[i])
				reason <- paste('more than max_cat',max_cat,sep=" ")
				Info_onDel <- c(Info_onDel, reason)
				}
			
			
			}
		
		}
	
	if(length(varhasnumeric)>=1){
		for(i in 1:length(varhasnumeric)){
			var_num <- as.numeric(DataIn[,varhasnumeric[i]])
			
			if(var(var_num)==0){
				vector_deL <- c(vector_deL,varhasnumeric[i])
				reason <- 'Zero variance!'
				Info_onDel <- c(Info_onDel, reason)
			}
		
		}
		
	}
	
	print(vector_deL)
	if(constVAR & !is.null(vector_deL)){
	    cat('This are variable having const variance or const value :\n',
			vector_deL,"\n")
	}
	
	if(constVAR & RemovconstVAR){
		### DataIn <- DataIn[,!(names(DataIn)%in% vector_deL)]
		}

    return(list(DataIn = DataIn, Delevars = vector_deL ,Info= Info_onDel))
	
}
