complete <- function(directory, id = 1:332) {
  
	setwd(file.path(getwd(), directory))
	nobs=numeric()
		
	for (i in id){
          
		if (i <10) { 
			data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),header = T, na.strings=c("NA","NaN", " "))
			nobs=c(nobs,sum(complete.cases(data)))
         }
      
		else if (i>=10 & i<100) { 
			data <- read.csv(paste("0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA","NaN", " ") )
			nobs=c(nobs,sum(complete.cases(data)))
		}

		else { 
			data <- read.csv(paste(as.character(i), ".csv", sep=""), header = T, na.strings=c("NA","NaN", " "))
			nobs=c(nobs,sum(complete.cases(data)))
	}
	
 	data = na.omit(data)    

}

  setwd("..")
  return(data.frame(id,nobs))

}
