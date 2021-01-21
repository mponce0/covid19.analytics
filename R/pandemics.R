# functions to retrieve pandemics historical data
# M.Ponce
# covid19.analytics

pandemics.loaddata <- function(tgt.file="pandemics.RDS",
			acknowledge=TRUE, show=FALSE,
			src='') {
#' internal function to retrieve historical data on pandemics
#'
#' @param  tgt.file  which data set to read
#' @param  acknowledge  displays details on the data sources
#' @param  show  displays data
#' @param  src
#'
#' @return data.frame
#'

	covid19.pckg <- 'covid19.analytics'

	pnds.RDS <- system.file("extdata",tgt.file, package=covid19.pckg, mustWork = TRUE)

	if (file.exists(pnds.RDS)) {
		pnd.data <- readRDS(pnds.RDS)
		if (acknowledge) message("Data obtained from",src)
		if (show) print(datatable(pnd.data))
		return(pnd.data)
	} else {
		stop("CRITICAL Error:", tgt.file, " missing!")
	}
}


pandemics.data <- function(acknowledge=TRUE, show=FALSE, tgt='pandemics') {
#' function to retrieve historical pandemics data
#'
#' @param  tgt  which data set to read -- options are  'pandemics' OR 'pandemics_vaccines'
#' @param  acknowledge  displays details on the data sources
#' @param  show  displays data
#'
#' @return data.frame
#'

	if (tgt=='pandemics') {
		src <- "Pandemic historical records -- data from  https://www.visualcapitalist.com/history-of-pandemics-deadliest/"
		tgt <- 'pandemics.RDS'
	} else if (tgt=='pandemics_vaccines') {
		src <- "Pandemics vaccine development times -- data from  https://www.visualcapitalist.com/the-race-to-save-lives-comparing-vaccine-development-timelines/"
		tgt <- 'pandemics_vaccines.RDS'
	} else {
		stop("Unrecognized option! Possible options for tgt are 'pandemics' OR 'pandemics_vaccines'")
	}

	return(pandemics.loaddata(tgt, acknowledge, show, src))
}
