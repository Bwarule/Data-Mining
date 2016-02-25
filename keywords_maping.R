
## Read the text file
df ## actual email text data
## res <- read.xlsx(file, 1)  # read first sheet

keywords ### keywords matrix mapping info
## res <- read.xlsx(file, 1)  # read first sheet
df$Weight <- NA
df$Category <- NA
stopWords <- stopwords("en")
class(stopWords)

for(i in 1:nrow(df)){
	Text_Body <- as.character(df$Text.Body[i])
	Text_Body <- gsub("[\r\n]", "", Text_Body)
	Text_Body <- tolower(Text_Body)
	str1 <-  strsplit(Text_Body, " ")
	str1 <-  unlist(str1)[!(unlist(str1) %in% c(stopWords,"date"))]
	Text_Body <- paste(str1[!(str1 %in% stopWords)],collapse=" ")
	Weight_old <- 0
	for(j in 1:nrow(keywords)){
		keywords_in <- unlist(t(keywords[j,-1]))
		row.names(keywords_in) <- NULL
		keywords_in <-  unlist(strsplit(keywords_in, " "))
		keywords_catgory <- as.character(keywords[j,1])
		getc <- unique(unlist(strsplit(Text_Body, " "))) %in% unlist(keywords_in)
		match_word <- unique(unlist(strsplit(Text_Body, " ")))[getc]
		Weight_new  <- length(match_word)
		if(j == 1 && length(match_word) >=1 ){
			Weight_old <- Weight_new
		}
		if(length(match_word) >=1 && Weight_old <= Weight_new){
		    if(Weight_old < Weight_new){
				df$Category[i] <- keywords_catgory
				}else{
				df$Category[i] <- paste(df$Category[i], keywords_catgory,sep=", ")
			}
			df$Category[i] <- gsub("NA,","",df$Category[i])
			Weight_old  <- Weight_new
			
		}
		
	}

}


