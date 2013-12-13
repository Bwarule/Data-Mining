cran_view <- read.csv('C:/Users/Documents/cran.csv')
mainDir <- "D:/R_packages"

library(ctv)
for(i in 1: dim(cran_view)[1]){
	
	subDir <- as.character(cran_view$view[i])
	subDir <- file.path(mainDir,subDir)
	if (file.exists(subDir)){
		## setwd(file.path(mainDir, subDir))
		} else {
		dir.create(file.path(mainDir, subDir))
    	## setwd(file.path(mainDir, subDir))
	}
	
	view_downlowed <- download.views(as.character(cran_view$view[i]),subDir)
	rm(subDir)
	gc()
	}
