tots.per.location <- function(data,geo.loc=NULL) {
#' function to compute totals per location
#'
#' @param  data  data.frame with data from covid19
#' @param  geo.loc  list of locations
#'
#' @return  a dataframe with totals per specified locations
#'
#' @export


	geo.loc <- checkGeoLoc(data,geo.loc)

	total.cases.per.country <- data.frame()

	quadrants <- min(5,as.integer(sqrt(length(geo.loc))))
	par(mfrow=c(quadrants,quadrants))
	#par(mfrow=c(2,1))
	cat(length(geo.loc), " -- ", quadrants, '\n')

	for (i in geo.loc) {
		if (i %in% toupper(data$Country.Region)) {
			cases.per.loc <- data[toupper(data$Country.Region) == i,]
		} else if (i %in% toupper(data$Province.State)) {
			cases.per.loc <- data[toupper(data$Province.State) == i,]
		}
		print(i)

		#totals.per.loc <- apply(cases.per.loc[,5:ncol(cases.per.loc)-1],MARGIN=2,sum)
		totals.per.loc <- sum(cases.per.loc[1:nrow(cases.per.loc),length(cases.per.loc)])

		#print(apply(totals.per.loc[,5:ncol(totals.per.loc)-1],MARGIN=2,sum))
		cat(i,' -- ',totals.per.loc,'\n')
		total.cases.per.country <- rbind(total.cases.per.country,cbind(i,totals.per.loc))

		col0 <- 5
		Ncols <- length(cases.per.loc)
		Nrows <- nrow(cases.per.loc)
		x.dates <- as.Date(names(cases.per.loc)[col0:Ncols])
		y.cases <- cases.per.loc[1:Nrows , col0:Ncols]
		names(y.cases) <- x.dates
		my.cols <- rep(rainbow(15L),each=20L)
		#print(x.dates)
		#print(y.cases)
		plot(unlist(y.cases), main=i, type='b', xlab="", axes=FALSE, pch=16L,col=my.cols)
		par(new=TRUE)
		barplot(unlist(y.cases), col = my.cols)
		par(new=FALSE)
	}

	#plot(unlist(data[ data$Country.Region==i ,5:52]))
	return(total.cases.per.country)

}


#############################################################################

set.plt.canvas <- function(geo.loc) {
#' fucntion to set the graphical layout
#'
#' @param geo.loc  list of locations
#'
#' @keywords internal
#'
	quadrants <- min(5,as.integer(sqrt(length(geo.loc))))
	par(mfrow=c(quadrants,quadrants))
	#par(mfrow=c(2,1))
	cat(length(geo.loc), " -- ", quadrants, '\n')

}


#############################################################################


checkGeoLoc <- function(data, geo.loc=NULL) {
#' fucntion to check the geographical location 
#'
#' @param  data  data.frame with data from covid19
#' @param  geo.loc  list of locations
#'
#' @return  list of capitalized locations within the data
#'
#' @keywords internal
#'

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


############################################################################


growth.rate <- function(data, geo.loc=NULL) {
#' function to compute Growth Rates
#'
#' @param  data  data.frame with data from covid19
#' @param  geo.loc  list of locations
#'
#'
#' @export

	col1 <- 6

	cluster.type <- c("ALL","Country","City","Region","City/Region")

	geo.loc <- checkGeoLoc(data,geo.loc)

	totals.per.day <- data.frame()

	set.plt.canvas(geo.loc)

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

		my.cols <- rep(rainbow(15L),each=20L)
		plot(as.Date(names(totals.per.loc[2:(nbr.of.days-1)])),growth.rate, axes=FALSE, ylim=c(0,max(growth.rate)*1.05), main=i, type='b', col=my.cols)
		par(new=TRUE)
		barplot(unlist(growth.rate), ylim=c(0,max(growth.rate)*1.05), col = my.cols)
		par(new=FALSE)

	}
	#names(totals.per.day)[1] <- "location"
	#names(totals.per.day)[2:nbr.of.days] <- names(totals.per.loc[2:nbr.of.days])
	totals.per.day <- growth.rate
	names(totals.per.day) <- names(totals.per.loc[2:(nbr.of.days-1)])

	
	return(totals.per.day)
}

#############################################################################
