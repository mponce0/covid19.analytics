
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

	# check that the data contain columns for Province.State/Country.Region
	cty.col <- pmatch("Country",names(data))
	prov.col <- pmatch("Province",names(data))

	critical.err <- "This could mean that the data structure has changed but it is a CRITICAL iissue, please contact the developer"

	message(" >>> checking data integrity...")

	if (is.na(cty.col))
		stop("Could not find in the data a column for Country/Region",'\n',
			critical.err)

	if(is.na(prov.col))
		stop("Could not find in the data a column for Province/City",'\n',
			crititcal.err)

	# conclusion
	msg <- paste("No critical issues have been found.",'\n')
	# recommendation...
	if (recommend)
		msg <- paste(msg, "Try also check for data consistency, using the consistency.check() function")

	# wrap-up message
	message(msg)
}

#######################################################################

consistency.check <- function(data, n0=5,nf=ncol(data), datasetName="") {
#' function that determines whether there are consistency issues within the data, such as, anomalies in the cumulative quantities of the data as reported by JHU/CCSEGIS
#'
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#'

	message(" >>> checking data consistency...")

	# check tha the data is TS...
	if (chk.TS.data(data)) {

		inconsis <- data.frame()

		for (i in 1:nrow(data)) {
			num.data <- as.numeric(data[i, n0:nf])
			deltas <- diff(num.data)
			inconsistencies <- deltas < 0

			if (sum(inconsistencies) > 0) 
				inconsis <- rbind(inconsis, data[i,c(1,2)])

		}

		if (nrow(inconsis) > 0) {
			warning("Inconsistency in ",datasetName," data detected -- the following ",nrow(inconsis)," records show inconsistencies in the data...", immediate. = TRUE)
			print(inconsis)
		}
	} else {
		message("This function applies to TimeSeries data only")
	}

}

#######################################################################

data.checks <- function(data,n0=5,nf=ncol(data), datasetName="") {
#' function to check for data integrity and data consistency
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#'

	# check for integrity
	integrity.check(data, datasetName, recommend=FALSE)

	# check for consistency
	consistency.check(data, n0,nf,datasetName)

}
