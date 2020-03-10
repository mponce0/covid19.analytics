tots.per.location <- function(data,geo.loc=NULL) {

	geo.loc <- checkGeoLoc(data,geo.loc)

	total.cases.per.country <- data.frame()

	for (i in geo.loc) {
		cases.per.loc <- data[toupper(data$Country.Region) == i,]
		#print(i)

		#totals.per.loc <- apply(cases.per.loc[,5:ncol(cases.per.loc)-1],MARGIN=2,sum)
		totals.per.loc <- sum(cases.per.loc[1:nrow(cases.per.loc),length(cases.per.loc)])

		#print(apply(totals.per.loc[,5:ncol(totals.per.loc)-1],MARGIN=2,sum))
		cat(i,' -- ',totals.per.loc,'\n')
		total.cases.per.country <- rbind(total.cases.per.country,cbind(i,totals.per.loc))
	}

	return(total.cases.per.country)

}

checkGeoLoc <- function(data, geo.loc=NULL) {

	provinces.states <- toupper(unique(data$Province.State))
	countries.regions <- toupper(unique(data$Country.Region))

	# if the geo.loc has not been specified will look into ALL records...
	if (is.null(geo.loc)) {
		geo.loc <- countries.regions
	} else {
		if (!(toupper(geo.loc) %in% provinces.states) & !(toupper(geo.loc) %in% countries.regions))
			stop("Unrecognized region: ",geo.loc)
	}

	return(toupper(geo.loc))
}

growth.rate <- function(data, geo.loc=NULL) {

	col1 <- 6

	cluster.type <- c("ALL","Country","City","Region","City/Region")

	geo.loc <- checkGeoLoc(data,geo.loc)

	totals.per.day <- data.frame()

	for (i in geo.loc) {
		cases.per.loc <- data[toupper(data$Country.Region) == i,]

		totals.per.loc <- apply(cases.per.loc[,col1:ncol(cases.per.loc)],MARGIN=2,sum)
		print(totals.per.loc)

		nbr.of.days <- length(totals.per.loc)
		changes <- totals.per.loc[2:nbr.of.days]-totals.per.loc[1:nbr.of.days-1]
		print(changes)

		growth.rate <- c()
		for (j in seq(1,length(changes)-1)) {
			if (changes[j] != 0) {
				growth.rate <- c(growth.rate, changes[j+1]/changes[j])
			} else {
				growth.rate <- c(growth.rate, 0)
			}
		}
		print(growth.rate)
		#totals.per.day <- rbind(totals.per.day, cbind(i,growth.rate))
	}
	#names(totals.per.day)[1] <- "location"
	#names(totals.per.day)[2:nbr.of.days] <- names(totals.per.loc[2:nbr.of.days])
	totals.per.day <- growth.rate
	names(totals.per.day) <- names(totals.per.loc[2:(nbr.of.days-1)])

	plot(as.Date(names(xxx)),xxx, type='b')
	
	return(totals.per.day)
}
