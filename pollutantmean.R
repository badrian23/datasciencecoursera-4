pollutantmean <- function(directory, pollutant, id = 1:332) {
  
	setwd(file.path(getwd(), directory))
	result = 0
	obs = 0
		
	for (i in id){
          
		if (i <10) { 
			data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),header = T, na.strings=c("NA","NaN", " "))
         }
      
		else if (i>=10 & i<100) { 
			data <- read.csv(paste("0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA","NaN", " ") )
		}

		else { 
			data <- read.csv(paste(as.character(i), ".csv", sep=""), header = T, na.strings=c("NA","NaN", " "))
	}
	
 	data = na.omit(data)    
	obs = obs + nrow(data)

	if (pollutant == "sulfate") {
		result = result + sum(data$sulfate)
	}
	else {
		result = result + sum(data$nitrate)
	}

}

  setwd("..")
  return (result/obs)

}
