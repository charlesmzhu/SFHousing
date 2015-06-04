temp <- list.files(pattern="*.csv")

# Empty list for storing all relevant data
data <- list()

#Vector of data
filesToCheck <- vector()

#For loading into environment and analysis
for (i in 1:(length(temp)) ) {
	set <- as.data.frame ( read.csv ( temp[i], header = TRUE ) )
	drops <- c("State", "Metro", "CountyName", "City")
	
	if ( is.na ( match ( "City", names( set ) ) ) ) {
		#Output to list of file names to check later
		filesToCheck <- c(filesToCheck, temp[i])
	} else {
		#Get only SF values and output to list	 
		set <- set[set$City == "San Francisco", ]
		data[[temp[i]]] <- as.data.frame(set)
		
		#Drops non-SF columns and makes RegionNames row.names
		a <- data[[temp[i]]]
		data[[temp[i]]] <- a[, !(names(a) %in% drops)]
		row.names(data[[temp[i]]]) <- data[[temp[i]]]$RegionName
		data[[temp[i]]] <- data[[temp[i]]][,-1]
		
		#Makes relevant substitutions within each data frame within the list
		b <- names(data[[temp[i]]])
		b <- gsub("X", "", b)
		b[grepl(".", b, fixed=TRUE)] <- gsub(".", "-", b[grepl(".", b, fixed=TRUE)], fixed = TRUE)
		b[grepl("-", b, fixed=TRUE)] <- paste(b[grepl("-", b, fixed=TRUE)], "-01", sep="")
		names(data[[temp[i]]]) <- b
		
		isTimeSeries <- try(as.Date(names(data[[temp[i]]])), silent = TRUE)
		if (length(isTimeSeries) < 1) {
			names(data[[temp[i]]]) <- as.Date(names(data[[temp[i]]]))
		}
	}
	
	#Makes relevants substitutions within the list
	names(data) <- gsub("Neighborhood", "", names(data))
	names(data) <- gsub("_", "", names(data))
	names(data) <- gsub(".csv", "", names(data))
}

writeToFile <- function ( list ) {
	for (i in 1:length(list)) {
		write.csv(list[[i]], paste(names(list[i]), "Cleaned", sep=""))
	}
}


###############################################################
writeToFile(data)

#For loading into csvs


names(test) <- gsub("Neighborhood_HomesSoldAsForeclosures-Ratio_AllHomes", "", names(test))

write.csv(fullNabeData, file="fullDataSet.csv", sep=",")

# Export big CSV file
nonDrops <- c("2014.11", "2014.12", "2015.01","2015.02","2015.03","2015.04")

#Get a master data set that's only the last year

#Change top date into Date objects. Make rownames the neighborhoods. Four pomodoros. Then work on app.