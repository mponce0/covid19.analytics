# Auxiliary fns of the covid19 package
#
# M.Ponce


#############################################################################

        
header0 <- function(x="-",title="",total.len=80,eol='\n') {
#' auxiliary fn to print "headers" adn 'titles'
#'
#' @param  x  character to use as lines
#' @param  title  title to dispo
#' @param  total.len  length of the line
#' @param  eol  end of line character
#'
#' @keywords internal
#'

	len.title <- nchar(title)
	if (len.title !=0) {
		reps <- (total.len-len.title)/2
		if (reps < 0) reps <- 1
		header(x,total.len=reps,eol=' ')
		cat(title)
		header(x,total.len=reps,eol='\n')
	} else {
		cat(paste(paste(rep(as.character(x),total.len),collapse=""),eol))
	}
}


#############################################################################

set.plt.canvas <- function(geo.loc,ylayers=1,minBreaks=5) {
#' auxiliary function to set the graphical layout
#'
#' @param  geo.loc  list of locations so that the fn can determine how many plots would be
#' @param  ylayers  parameter to set a multiplier times the nbr of initial plots
#' @param  minBreaks  cut-off to set the minimum nbr of plots
#'
#' @keywords internal
#'
	quadrants <- min(minBreaks,ceiling(as.integer(sqrt(length(geo.loc)))))
	#print(quadrants)
	#print(ylayers)
	par(mfrow=c(quadrants*ylayers,quadrants))
	#par(mfrow=c(2,1))
	#cat(length(geo.loc), " -- ", quadrants, '\n')

}


#############################################################################


select.per.loc <- function(data,geo.loc) {
#' auxiliary function to select data based on geographical location
#'
#' @param  data  data set to process
#' @param  geo.loc  geopgraphical location, can be a country, region, province or city
#'
#' @keywords internal
#'

	# generalize Country/Region & Province/State column id
	Country.col <- pmatch("Country", names(data))
	Province.col <- pmatch("Province", names(data))

geo.loc0<-geo.loc
cases.per.loc0 <- data.frame()
for (geo.loc in geo.loc0) {
	# check whether the locations are coutnries/regions or provinces/states
	if (toupper(geo.loc) %in% toupper(data[,Country.col])) {
		cases.per.loc <- data[toupper(data[,Country.col]) == toupper(geo.loc),]
	} else if (toupper(geo.loc) %in% toupper(data[,Province.col])) {
		cases.per.loc <- data[toupper(data[,Province.col]) == toupper(geo.loc),]
	} else if (toupper(geo.loc) == "ALL") {
		cases.per.loc <- data
	}
cases.per.loc0 <- rbind(cases.per.loc0,cases.per.loc)
}
return(cases.per.loc0)
	#print(geo.loc)

	return(cases.per.loc)
}

#######################################################################

checkGeoLoc <- function(data, geo.loc=NULL) {
#' function to check the geographical location 
#'
#' @param  data  data.frame with data from covid19
#' @param  geo.loc  list of locations
#'
#' @return  list of capitalized locations within the data
#'
#' @keywords internal
#'

	col.names <- names(data)
	#print(col.names)

        # generalize Country/Region & Province/State column id
        Country.col <- pmatch("Country", col.names)
        Province.col <- pmatch("Province", col.names)

	#print(Country.col)
	#print(Province.col)
	if (is.na(Country.col) & is.na(Province.col))
		stop("Error in data structure - could not find columns with geographical information (Province/Country)!")

	provinces.states <- toupper(unique(data[,Province.col]))
	countries.regions <- toupper(unique(data[,Country.col]))
	geo.locs <- c()

	# if the geo.loc has not been specified will look into ALL records...
	if (is.null(geo.loc)) {
		geo.loc <- countries.regions
	} else {
		for (geo.ind in geo.loc) {
			if (!(toupper(geo.ind) %in% provinces.states) & !(toupper(geo.ind) %in% countries.regions) & !(toupper(geo.ind) == "ALL") ) {
				warning(paste("Unrecognized region: ",geo.loc," will skip it!"))
			} else {
				geo.locs <- c(geo.locs,geo.ind)
			}
		}

		if (length(geo.locs) >= 1) {
			geo.loc <- geo.locs
		} else {
			message("Unrecognized location: ",geo.loc)
			message("Possible options are:")
			print(unlist(provinces.states))
			print(unlist(countries.regions))
			stop()
		}
	}

	return(toupper(geo.loc))
}


############################################################################

chk.TS.data <- function(data, xtp=FALSE) {
#' auxiliary function to check whether a dataset is composed of time series data
#'
#' @param  data  data set to consider
#' @param  xtp  indicator whether to stop the program if data is not time series
#'
#' @return  a boolean indicator whether the data contains time series values or not
#'
#' @keywords internal

	# define some unique column names to aggregated data
	agg.cols <- c("FIPS","Last_Update")
	
	if ( sum(agg.cols %in% names(data)) >0 ) {
		if (xtp) {
			stop("Data provided does not contain *time series* data!")
		} else {
			return(FALSE)
		}
	} else {
		return(TRUE)
	}
}


############################################################################

preProcessingData <- function(data0,geo.loc){
#' auxiliary function to pre-process data per geographical location
#'
#' @param  data0  data set
#' @param  geo.loc  geopgraphical location, can be a country, region, province or city
#'
#' @keywords internal
#'

        # define first column of data
        col1 <- 5

        # check on the location
        geo.loc <- checkGeoLoc(data0,geo.loc)


        if ("status" %in% names(data0)) {
#                data <- data0[, ! names(data0) %in% "status", drop = F]
		data <- data0[ data$status == "confirmed",! names(data0) %in% "status", drop = F]
        } else {
                data <- data0
        }

	cases.per.loc <- select.per.loc(data,geo.loc)
	if (nrow(cases.per.loc) > 1) {
		#cases.per.loc <- cbind("*",cases.per.loc$Country.Region,mean(cases.per.loc$Lat),mean(cases.per.loc$Long),
		cases.per.loc <- apply(cases.per.loc[,col1:ncol(cases.per.loc)],MARGIN=2,sum)
		#cases.per.loc["Country"] <- cases.per.loc$Country[1]
		#cases.per.loc["Lat"] <- mean(cases.per.loc$Lat)
		#cases.per.loc["Long"] <- mean(cases.per.loc$Long)
	} else {
		cases.per.loc <- cases.per.loc[,col1:ncol(cases.per.loc)]
	}
	colN <- length(cases.per.loc)

	#if (tolower("status") %in% tolower(cases.per.loc))
	#	colN <- colN - 1

	cat("Processing... ",geo.loc,'\n')
	# determine period of time and ranges...
	stride <- 1
	range1 <- seq(1,colN,stride)
	return(as.numeric(unlist(cases.per.loc[range1])))
}


##########################################################################


# load and check needed packages/libraries...
loadLibrary <- function(lib) {
#' auxiliary function to check and load an specific set of libraries
#' @param  lib  is a list of packages to be loaded
#' @keywords internal

        if (require(lib,character.only = TRUE) == FALSE) stop(lib, " is needed for this package to work but is not installed in your system!")
}


#########################################################################
