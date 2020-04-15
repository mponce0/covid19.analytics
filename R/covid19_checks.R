
# Consitency and integrity check fns of the covid19.analytics package
#
# M.Ponce


#######################################################################

integrity.check <- function(data, datasetName="", recommend=TRUE) {
#' function that determines whether there are integrity issues within the datasets
#' or changes to the structure of the data as reported by JHU/CCSEGIS
#'
#' @param  data  dataset to analyze
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  recommend  optional flag to recommend further actions
#'

	chck.cols <- function(col.names,cols) {

		critical.err <- FALSE

		for (i in cols) {
			x.col <- pmatch(i, col.names)
			if (is.na(x.col)) {
				warning("CRITICAL: Could not find in the data a column for ", i)
				critical.err <- TRUE
			}
		}

		# conclusion...
		if (critical.err) {
			critical.err.msg <- "This could mean that the data structure has changed but it is a CRITICAL issue, please contact the developer"
			stop(critical.err.msg)
		} else {
			message("No critical issues have been found.")
		}

	}

	#########

	message(" >>> checking data integrity...")

	# define critical columns to check for
	critical.cols <- c("Country","Province","Lat","Long")
	# check that the data contain columns for the criitical cols defined above
	chck.cols(names(data), critical.cols)

	# wrapup msg - recommendation...
	if (recommend)
		message("Try also to check for data consistency, using the consistency.check() function")
}

#######################################################################

consistency.check <- function(data, n0=5,nf=ncol(data), datasetName="", details=TRUE) {
#' function that determines whether there are consistency issues within the data, such as, anomalies in the cumulative quantities of the data as reported by JHU/CCSEGIS
#'
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  details  optional argument to specify whether to show details about the records where inconsistencies were detected
#'

	message(" >>> checking data consistency...")

	# check tha the data is TS...
	if (chk.TS.data(data)) {

		# check if 'status' is in a column, ie. combined TS data
		sts.col <- pmatch("status",tolower(names(data)))
		if (!is.na(sts.col))
			nf <- sts.col - 1

		inconsis <- data.frame()

		for (i in 1:nrow(data)) {
			num.data <- as.numeric(data[i, n0:nf])
			deltas <- diff(num.data)
			inconsistencies <- deltas < 0

			if (sum(inconsistencies, na.rm=TRUE) > 0) 
				inconsis <- rbind(inconsis, data[i,c(1,2)])

		}

		if (nrow(inconsis) > 0) {
			warning("Inconsistency in ",datasetName," data detected -- ",nrow(inconsis)," records (out of ",nrow(data),") show inconsistencies in the data...", immediate. = TRUE)
			if (details) print(inconsis)
		}
	} else {
		message("This function applies to TimeSeries data only")
	}

}

#######################################################################

data.checks <- function(data,n0=5,nf=ncol(data), datasetName="", details=TRUE) {
#' function to check for data integrity and data consistency
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  details  optional argument to specify whether to show details about the records where inconsistencies were detected
#'

	# check for integrity
	integrity.check(data, datasetName, recommend=FALSE)

	# check for consistency
	consistency.check(data, n0,nf,datasetName, details)

}

#######################################################################

