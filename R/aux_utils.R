# Auxiliary fns of the covid19 package
#
# M.Ponce


#############################################################################

        
header <- function(x="-",title="",total.len=80,eol='\n') {
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
		header(x,total.len=reps,eol=' ')
		cat(title)
		header(x,total.len=reps,eol='\n')
	} else {
		cat(paste(paste(rep(as.character(x),total.len),collapse=""),eol))
	}
}


#############################################################################

set.plt.canvas <- function(geo.loc,ylayers=1,minBreaks=5) {
#' function to set the graphical layout
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

	# check whether the locations are coutnries/regions or provinces/states
	if (toupper(geo.loc) %in% toupper(data$Country.Region)) {
		cases.per.loc <- data[toupper(data$Country.Region) == toupper(geo.loc),]
	} else if (toupper(geo.loc) %in% toupper(data$Province.State)) {
		cases.per.loc <- data[toupper(data$Province.State) == toupper(geo.loc),]
	} else if (toupper(geo.loc) == "ALL") {
		cases.per.loc <- data
	}

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

	provinces.states <- toupper(unique(data$Province.State))
	countries.regions <- toupper(unique(data$Country.Region))
	geo.locs <- c()

	# if the geo.loc has not been specified will look into ALL records...
	if (is.null(geo.loc)) {
		geo.loc <- countries.regions
	} else {
		for (geo.ind in geo.loc) {
			if (!(toupper(geo.ind) %in% provinces.states) & !(toupper(geo.ind) %in% countries.regions) & !(toupper(geo.ind) == "ALL") ) {
				cat(paste("Unrecognized region: ",geo.loc," will skip it!",'\n'))
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

preProcessingData <- function(data0,geo.loc){

        # define first column of data
        col1 <- 6

        # check on the location
        geo.loc <- checkGeoLoc(data0,geo.loc)


        if ("status" %in% names(data0)) {
                data <- data0[, ! names(data0) %in% "status", drop = F]
        } else {
                data <- data0
        }

	cases.per.loc <- select.per.loc(data,geo.loc)
	colN <- ncol(cases.per.loc)
	if (tolower("status") %in% tolower(cases.per.loc))
		colN <- colN - 1

	cat("Processing... ",geo.loc,'\n')


	# determine period of time and ranges...
	stride <- 1
	range1 <- seq(col1,colN,stride)

	return(as.numeric(unlist(cases.per.loc[range1])))
}


##########################################################################


# load and check needed packages/libraries...
loadLibrary <- function(lib) {
#' function to check and load an specific set of libraries
#' @param  lib  is a list of packages to be loaded
#' @keywords internal

        if (require(lib,character.only = TRUE) == FALSE) stop(lib, " is needed for this package to work but is not installed in your system!")
}


#########################################################################
