
# Consitency and integrity check fns of the covid19.analytics package
#
# M.Ponce


#######################################################################

integrity.check <- function(data, datasetName="", disclose=FALSE, recommend=TRUE) {
#' function that determines whether there are integrity issues within the datasets
#' or changes to the structure of the data as reported by JHU/CCSEGIS
#'
#' @param  data  dataset to analyze
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  disclose  boolean flag to indicate whether index of problematic entries are returned 
#' @param  recommend  optional flag to recommend further actions
#'
#' @export

	chck.cols <- function(col.names,cols) {

		critical.err <- FALSE

		message("checking for ... ", paste(cols, collapse=' '))
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

	chck.cols.qty <- function(ccols, data, disclose=FALSE) {

		disclose.entries <- c()
		col.names <- names(data)
		for (i in ccols) {
			x.col <- pmatch(i, col.names)
			chk <- data[,x.col] < 0
			if ( sum(chk, na.rm=T) != 0 ) {
				warning("Column ",i," has ",length(which(chk))," entries reporting negative values!",'\n',
					'\t ',"on entries: ",paste(which(chk),collapse=' '))
				disclose.entries <- c(disclose.entries, which(chk))
			}
			
		}

		# check for rounded quantities
		if ( sum(ccols %in% col.names)==length(ccols) ) {
			# condition for NOT matching nbrs
			condn <- data$Active + data$Recovered + data$Deaths != data$Confirmed

			if  ( sum(condn, na.rm=TRUE) !=0 ) {
				warning("number of 'active+recovered+deaths' cases does NOT match the number of 'confirmed' cases!",'\n',
					'\t',"on ", length(which(condn))," entries -- ", paste(which(condn),collapse=' '), '\n',
					'\t ||  ',paste(colnames(data),collapse='  '),'\n',
					"  ", paste(which(condn),collapse=' '), ' || ', paste(data[which(condn),],collapse='  '))
				disclose.entries <- c(disclose.entries, which(condn))
			}
		}

		if (disclose)
			if (length(disclose.entries)>0) return(unique(disclose.entries))
	}

	#########

	message(" >>> checking data integrity...")

	### Check for column names
	# define critical columns to check for
	critical.cols <- c("Country","Province","Lat","Long")
	# check that the data contain columns for the criitical cols defined above
	chck.cols(names(data), critical.cols)

	### Check for aggregated columns: Active, Deaths, Recovered, Confirmed
	agg.critical.cols <- c("Active","Deaths","Recovered","Confirmed")
	if (sum(agg.critical.cols %in% names(data)) > 0) {
		message("Possible <<Aggregated data-type>> detected...")
		chck.cols(names(data), agg.critical.cols)

		# check for quantitative issues
		idx.to.disclose <- chck.cols.qty(agg.critical.cols, data, disclose=disclose)
	}

	# wrap-up msg - recommendation...
	if (recommend)
		message("Try also to check for data consistency, using the consistency.check() function")

	# return row index of suspicious problematic entries
	if (disclose) if("idx.to.disclose" %in% ls()) return(idx.to.disclose)
}

#######################################################################

consistency.check <- function(data, n0=5,nf=ncol(data), datasetName="", disclose=FALSE, details=TRUE) {
#' function that determines whether there are consistency issues within the data, such as, anomalies in the cumulative quantities of the data as reported by JHU/CCSEGIS
#'
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  disclose  boolean flag to indicate whether index of problematic entries are returned
#' @param  details  optional argument to specify whether to show details about the records where inconsistencies were detected
#'
#' @export

	message(" >>> checking data consistency...")

	# check tha the data is TS...
	if (chk.TS.data(data)) {

		# check if 'status' is in a column, ie. combined TS data
		sts.col <- pmatch("status",tolower(names(data)))
		if (!is.na(sts.col))
			nf <- sts.col - 1

		inconsis.I <- data.frame()
		inconsis.II <- data.frame()

		for (i in 1:nrow(data)) {
			num.data <- as.numeric(data[i, n0:nf])
			deltas <- diff(num.data)
			# Inconsistencies Type I: negative values
			inconsistency.type.I <- num.data < 0
			# Inconsistencies Type I: decreasing cumulative values
			inconsistency.type.II <- deltas < 0

			if (sum(inconsistency.type.I, na.rm=TRUE) > 0) 
				inconsis.I <- rbind(inconsis.I, data[i,c(1,2)])

			if (sum(inconsistency.type.II, na.rm=TRUE) > 0)
				inconsis.II <- rbind(inconsis.II, data[i,c(1,2)])
		}
		inconsistencies <- list(type.I=inconsis.I, type.II=inconsis.II)
		incons <- as.numeric(c(rownames(inconsistencies$type.I), rownames(inconsistencies$type.II)))

		for (i in seq_along(inconsistencies)) {
			if (nrow(inconsistencies[[i]]) > 0) {
				warning("Inconsistency of ",names(inconsistencies)[i] ," in ",datasetName," data detected -- ",nrow(inconsistencies[[i]])," records (out of ",nrow(data),") show inconsistencies in the data...", immediate. = TRUE)
				if (details) print(inconsistencies[[i]])
			}
		}
		
	} else {
		message("This function applies to TimeSeries data only")
	}

	if (disclose)
		if ("incons" %in% ls()) return(incons)
}

#######################################################################

data.checks <- function(data,n0=5,nf=ncol(data), datasetName="", details=TRUE, disclose=FALSE) {
#' function to check for data integrity and data consistency
#' @param  data  dataset to analyze
#' @param  n0  column where the cumulative data begins
#' @param  nf  column where the cumulative data ends
#' @param  datasetName  optional argument to display the name of the dataset
#' @param  disclose  boolean flag to indicate whether index of problematic entries are returned
#' @param  details  optional argument to specify whether to show details about the records where inconsistencies were detected
#'
#' @export

	# check for integrity
	integrity.check(data, datasetName, recommend=FALSE)

	# check for consistency
	consistency.check(data, n0,nf,datasetName, details=details,disclose=disclose)

}

#######################################################################

nullify.data <- function(data,stringent=FALSE) {
#' remove inconsistencies from data by removing 'suspicious' entries
#'
#' @param  data  dataset to process
#' @param  stringent  only return records with "complete cases"
#'
#' @importFrom  stats  complete.cases
#'
#' @export
#'

	orig.recs <- nrow(data)
	# aggregated data-type
	susp.entries <- integrity.check(data, disclose=TRUE, recommend=FALSE)

	susp.recs <- length(susp.entries)
	if (susp.recs > 0) {
		data <- data[-susp.entries,]
		message("*** ", susp.recs," entries were removed due to data inconsistences")
	}

	# TS data-type
	ts.susp.entries <- consistency.check(data, disclose=TRUE, details=FALSE)
	ts.recs <- length(ts.susp.entries)
	if (ts.recs > 0) {
		data <- data[-ts.susp.entries,]
		message("*** ", ts.recs," entries were removed due to data inconsistences")
#message(nrow(data))
	}

	if (stringent) {
		data <- data[complete.cases(data),]

		upd.recs <- nrow(data)
		diff.recs <- (orig.recs-susp.recs-ts.recs)-upd.recs
#message(orig.recs," - ",susp.recs," - ",ts.recs," - ",upd.recs," = ", diff.recs)
		if (diff.recs != 0)
			message("*** ", diff.recs," entries were removed due to data incompleteness")
	}

	return(data)
}

#######################################################################

