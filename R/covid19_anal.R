# Analysis fns of the covid19 package
#
# M.Ponce


#######################################################################

tots.per.location <- function(data,geo.loc=NULL) {
#' function to compute totals per location
#'
#' @param  data  data.frame with data from covid19
#' @param  geo.loc  list of locations
#'
#' @return  a dataframe with totals per specified locations
#'
#' @export
#'
#' @examples
#' # read data for confirmed cases
#' data <- covid19("confirmed")
#' # compute totals per location for all the countries
#' tots.per.location(data)
#' # compute totals per location for 'Italy'
#' tots.per.location(data,geo.loc="Italy")
#' # compute totals per location for 'Italy' and 'Germany'
#' tots.per.location(data,geo.loc=c("Italy","Germany"))

	# first column with daily data
	col1 <- 6

	# check the location indicated
	geo.loc <- checkGeoLoc(data,geo.loc)

	# dataframe to store results
	total.cases.per.country <- data.frame()

	# set some graphical parameters
	set.plt.canvas(geo.loc)

	for (i in geo.loc) {
		cases.per.loc <- select.per.loc(data,i)

		totals.per.loc.day <- apply(cases.per.loc[,col1:ncol(cases.per.loc)],MARGIN=2,sum)
		print(totals.per.loc.day)
		if (toupper(i) != "ALL") {
			totals.per.loc <- sum(cases.per.loc[1:nrow(cases.per.loc),length(cases.per.loc)])
		} else {
			totals.per.loc <- apply(cases.per.loc[,col1:ncol(cases.per.loc)],MARGIN=2,sum)
		}
		cat(i,' -- ',totals.per.loc,'\n')
		total.cases.per.country <- rbind(total.cases.per.country,c(i,totals.per.loc.day,totals.per.loc))

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
		barplot(unlist(y.cases), main=i, col = my.cols)
		par(new=FALSE)
	}

	#plot(unlist(data[ data$Country.Region==i ,5:52]))
	return(total.cases.per.country)

}


#############################################################################


growth.rate <- function(data0, geo.loc=NULL, stride=1) {
#' function to compute Growth Rates
#'
#' @param  data0  data.frame with data from covid19
#' @param  geo.loc  list of locations
#' @param  stride  how frequently to compute the growth rate in units of days
#'
#'
#' @export
#'
#' @examples
#' # read data for confirmed cases
#' data <- covid19("confirmed")
#' # compute changes and growth rates per location for all the countries
#' growth.rate(data)
#' # compute changes and growth rates per location for 'Italy'
#' growth.rate(data,geo.loc="Italy")
#' # compute changes and growth rates per location for 'Italy' and 'Germany'
#' growth.rate(data,geo.loc=c("Italy","Germany"))

	# define first column of data
	col1 <- 6

	cluster.type <- c("ALL","Country","City","Region","City/Region")

	# check on the location
	geo.loc <- checkGeoLoc(data,geo.loc)

	# where to store the results
	totals.per.day <-data.frame() 

	# set graphical output parameters
	set.plt.canvas(geo.loc,2)

	if ("status" %in% names(data0)) {
		data <- data0[, ! names(data0) %in% "status", drop = F]
	} else {
		data <- data0
	}

	for (i in geo.loc) {
		cases.per.loc <- select.per.loc(data,i)

		# check whether the locations are coutnries/regions or provinces/states
#		if (i %in% toupper(data$Country.Region)) {
#			cases.per.loc <- data[toupper(data$Country.Region) == i,]
#		} else if (i %in% toupper(data$Province.State)) {
#			cases.per.loc <- data[toupper(data$Province.State) == i,]
#		}
		cat("Processing... ",i,'\n')

		totals.per.loc <- apply(cases.per.loc[,col1:ncol(cases.per.loc)],MARGIN=2,sum)
		#print(totals.per.loc)

		# determine period of time and ranges...
		nbr.of.days <- length(totals.per.loc)
		range1 <- seq(1,nbr.of.days-1,stride)
		range2 <- range1+1

		# compute changes...
		changes <- totals.per.loc[range2]-totals.per.loc[range1]
		#print(changes)

		# compute growth rate
		gr.rate <- changes[range2]/changes[range1]
		# remove infinites in case of divison by 0 or something tiny...
		gr.rate[gr.rate == "Inf"] <- NA
		print(gr.rate)

		# update resulting dataframe
		subTotals.per.day <- c(i,as.numeric(unlist(gr.rate)))
		names(subTotals.per.day) <- c(1:length(subTotals.per.day))	#("location",names(changes[range1]))
		print(names(subTotals.per.day))
		totals.per.day <- rbind(totals.per.day,subTotals.per.day, deparse.level=0)
		names(totals.per.day) <- c(1:length(subTotals.per.day))	#("location",names(changes[range1]))
		#print(subTotals.per.day)

		# some graphic output...		
		my.cols <- rep(rainbow(15L),each=20L)
		x.dates <- as.Date(names(totals.per.loc[2:length(totals.per.loc)]))
		plot(x.dates,changes, type='b', main=i, xlab="time",ylab="Nbr of Changes", col=my.cols)
		plot(x.dates,gr.rate, axes=FALSE,xlab='',ylab='', ylim=c(0,max(gr.rate,na.rm=TRUE)*1.05), main=i, type='b', col=my.cols)
		par(new=TRUE)
		barplot(unlist(gr.rate), ylab="Growth Rate",xlab="Time",col = my.cols)
		par(new=FALSE)

	}

	#names(totals.per.day)[1] <- "location"
	#names(totals.per.day)[2:length(totals.per.day)] <- names(changes[range1])
	#totals.per.day <- gr.rate
	#names(totals.per.day) <- names(totals.per.loc[seq(2,(nbr.of.days-1),stride)])

	
	return(totals.per.day)
}

#############################################################################
