# Auxiliary fns of the covid19 package
#
# M.Ponce



#############################################################################

set.plt.canvas <- function(geo.loc,ylayers=1) {
#' fucntion to set the graphical layout
#'
#' @param geo.loc  list of locations
#'
#' @keywords internal
#'
	quadrants <- min(5,as.integer(sqrt(length(geo.loc))))
	par(mfrow=c(quadrants*ylayers,quadrants))
	#par(mfrow=c(2,1))
	cat(length(geo.loc), " -- ", quadrants, '\n')

}


#############################################################################


select.per.loc <- function(data,geo.loc) {

	# check whether the locations are coutnries/regions or provinces/states
	if (toupper(geo.loc) %in% toupper(data$Country.Region)) {
		cases.per.loc <- data[toupper(data$Country.Region) == toupper(geo.loc),]
	} else if (toupper(geo.loc) %in% toupper(data$Province.State)) {
		cases.per.loc <- data[toupper(data$Province.State) == toupper(geo.loc),]
	}

	print(geo.loc)

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
			if (!(toupper(geo.ind) %in% provinces.states) & !(toupper(geo.ind) %in% countries.regions)) {
				cat(paste("Unrecognized region: ",geo.loc," will skip it!",'\n'))
			} else {
				geo.locs <- c(geo.locs,geo.ind)
			}
		}
		if (length(geo.locs) < 1) geo.loc <- geo.locs
	}

	return(toupper(geo.loc))
}


############################################################################



##########################################################################


# load and check needed packages/libraries...
loadLibrary <- function(lib) {
#' function to check and load an specific set of libraries
#' @param  lib  is a list of packages to be loaded
#' @keywords internal

        if (require(lib,character.only = TRUE) == FALSE) stop(lib, " is needed for this package to work but is not installed in your system!")
}


#########################################################################
