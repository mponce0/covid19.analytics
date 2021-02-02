
# Main data acquisition fns of the covid19.analytics package
#
# M.Ponce


#######################################################################

covid19.data <- function(case='aggregated', local.data=FALSE, debrief=FALSE, acknowledge=FALSE) {
#' function to read "live" data from reported covid19 cases
#'
#' @param  case  a string indicating the category of the data, possible values are:
#'      "aggregated" :  latest number of cases *aggregated* by country,
#'      "ts-confirmed" :  time data of confirmed cases,
#'      "ts-deaths"    :  time series data of fatal cases,
#'      "ts-recovered" :  time series data of recovered cases,
#'      "ts-ALL"       :  all time series data combined,
#'      "ts-confirmed-US"  : time series data of confirmed cases for the United States,
#'      "ts-deaths-US"     : time series data of fatal cases for the United States,
#'      "ts-dep-confirmed" : time series data of confirmed cases as originally reported (depricated),
#'      "ts-dep-deaths"    : time series data of deaths as originally reported (depricated),
#'      "ts-dep-recovered" : time series data of recovered cases as originally reported (depricated),
#'      "ALL": all of the above
#'      "ts-Toronto"       :  data for the City of Toronto, ON - Canada
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe (or a list in the case of "ALL") with the daily worlwide indicated type of data per country/region/city
#'
#' @export
#'
#' @examples
#'\donttest{
#' # reads all possible datastest, returnin a list
#' covid19.all.datasets <- covid19.data("ALL")
#' }
#' # reads the latest aggregated data
#' covid19.ALL.agg.cases <- covid19.data("aggregated")
#' # reads time series data for casualities
#' covid19.TS.deaths <- covid19.data("ts-deaths")
#'

	if (tolower(case) == "ts-toronto") {
		# data from the the City of Toronto
		return(covid19.Toronto.data(data.fmt="TS",local.data=local.data,debrief=debrief,acknowledge=acknowledge))
	} else {
		# data coming from JHU
		return(covid19.JHU.data(case,local.data,debrief,acknowledge))
	}
}


######################################################################


debriefing <- function(data,debrief=TRUE) {

	if (debrief) {
		print(str(data))
		print(head(data))
		print(names(data))
	}
}


######################################################################


covid19.JHU.data <- function(case='aggregated', local.data=FALSE, debrief=FALSE, acknowledge=FALSE) {
#' function to read "live" data as reported by JHU's CCSE repository
#'
#' @param  case  a string indicating the category of the data, possible values are:
#'	"aggregated" :  latest number of cases *aggregated* by country,
#'	"ts-confirmed" :  time data of confirmed cases,
#'	"ts-deaths"    :  time series data of fatal cases,
#'	"ts-recovered" :  time series data of recovered cases,
#'	"ts-ALL"       :  all time series data combined,
#'	"ts-confirmed-US"  : time series data of confirmed cases for the United States,
#'	"ts-deaths-US"     : time series data of fatal cases for the United States,
#'	"ts-dep-confirmed" : time series data of confirmed cases as originally reported (depricated),
#'	"ts-dep-deaths"    : time series data of deaths as originally reported (depricated),
#'	"ts-dep-recovered" : time series data of recovered cases as originally reported (depricated),
#'	"ALL": all of the above
#'	"Toronto"	:  data for the City of Toronto, ON - Canada
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe (or a list in the case of "ALL") with the daily worlwide indicated type of data per country/region/city
#'
#' @importFrom utils  read.csv
#'
##' @export
##'
##' @examples
##'\donttest{
##' # reads all possible datastest, returnin a list
##' covid19.all.datasets <- covid19.data("ALL")
##' }
##' # reads the latest aggregated data
##' covid19.ALL.agg.cases <- covid19.data("aggregated")
##' # reads time series data for casualities
##' covid19.TS.deaths <- covid19.data("ts-deaths")
##'

	###############################

	## function for error handling
	errorHandling.Msg <- function(condition,target.case) {
		header('=')
		message("A problem was detected when trying to retrieve the data for the package: ",target.case)
		if (grepl("404 Not Found",condition)) {
			message("The URL or file was not found! Please contact the developer about this!")
		} else {
			message("It is possible that your internet connection is down! Please check!")
                }
		message(condition,'\n')
		header('=')

                # update problems counter
                #pkg.env$problems <- pkg.env$problems + 1
        }

        ###############################

	## function for combining file for time series data
	get.comb.TS <- function(dep="", local.data) {

		# comnfirmed cases
		covid19.conf <- covid19.data(paste0("ts-",dep,"confirmed"), local.data)
                covid19.conf <- cbind(covid19.conf, "confirmed")
                names(covid19.conf)[length(covid19.conf)] <- "status"
		ncol.conf <- ncol(covid19.conf)

		# death cases
                covid19.death <- covid19.data(paste0("ts-",dep,"deaths"), local.data)
                covid19.death <- cbind(covid19.death, "death")
                names(covid19.death)[length(covid19.death)] <- "status"
		ncol.death <- ncol(covid19.death)

		# recovered cases
                covid19.recov <- covid19.data(paste0("ts-",dep,"recovered"), local.data)
                covid19.recov <- cbind(covid19.recov, "recovered")
                names(covid19.recov)[length(covid19.recov)] <- "status"
		ncol.recov <- ncol(covid19.recov)

		min.cols <- min(ncol.conf-1,ncol.death-1,ncol.recov-1)

		#print(min.cols)
		# combine cases
                covid19.ts <- rbind( covid19.conf[,c(1:min.cols, ncol.conf)],
					covid19.death[,c(1:min.cols, ncol.death)],
					covid19.recov[,c(1:min.cols, ncol.recov)] )

		# check consitency of the data
		#consistency.check(covid19.ts,datasetName="combined Time Series", details=FALSE)

		return(covid19.ts)
	}

	###############################

	# URL JHU's CCSE repository
	JHU.repo <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/"
	# TimeSeries URL
	TS.repo <- paste0(JHU.repo,"csse_covid_19_time_series/")
	# AGGregated URL
	AGG.repo <- paste0(JHU.repo,"csse_covid_19_daily_reports/")

	if (toupper(case) == "ALL") {
		#### aggregated
		covid19.agg <- covid19.data("aggregated", local.data)

		### Time Series
		#covid19.conf <- covid19.data("ts-confirmed", local.data)
                #covid19.conf <- cbind(covid19.conf, "confirmed")
		#names(covid19.conf)[length(covid19.conf)] <- "status"
                #covid19.death <- covid19.data("ts-deaths", local.data)
                #covid19.death <- cbind(covid19.death, "death")
                #names(covid19.death)[length(covid19.death)] <- "status"
		#covid19.recov <- covid19.data("ts-recovered", local.data)
                #covid19.recov <- cbind(covid19.recov, "recovered")
                #names(covid19.recov)[length(covid19.recov)] <- "status"
		#
		#covid19.ts <- rbind(covid19.conf,covid19.death,covid19.recov)
		covid19.ts <- get.comb.TS("",local.data)

		#### depricated
		#covid19.conf <- covid19.data("ts-dep-confirmed", local.data)
		#covid19.conf <- cbind(covid19.conf, "confirmed")
		#names(covid19.conf)[length(covid19.conf)] <- "status"
		#covid19.death <- covid19.data("ts-dep-deaths", local.data)
		#covid19.death <- cbind(covid19.death, "death")
		#names(covid19.death)[length(covid19.death)] <- "status"
		#covid19.recov <- covid19.data("ts-dep-recovered", local.data)
		#covid19.recov <- cbind(covid19.recov, "recovered")
		#names(covid19.recov)[length(covid19.recov)] <- "status"
		#
		#covid19.dep <- rbind(covid19.conf,covid19.death,covid19.recov)
		covid19.dep <-get.comb.TS("dep-",local.data=TRUE)

		return(list('aggregated'=covid19.agg, 'time.series'=covid19.ts, 'ts.dep'=covid19.dep))
	} else if(toupper(case) == "TS-ALL") {
		### Time Series data
		covid19.ts <- get.comb.TS("",local.data)
		return(covid19.ts)
	} else {


	possible.cases <- c("aggregated",
				# TimeSeries
				"ts-confirmed","ts-deaths", "ts-recovered", "ts-ALL",
				# depricated
				"ts-dep-confirmed","ts-dep-deaths","ts-dep-recovered",
				"ALL",
				# TimeSeries US & Toronto
				"ts-confirmed-us","ts-deaths-us",
				"ts-Toronto"
				)
	if (! tolower(case) %in% possible.cases)
		stop("Unrecognized selection of case <",case,"> -- possible options are: ",paste(possible.cases,collapse=" "))


	if (!local.data) {
	# filename for corresponding cases
	cases <- switch(tolower(case),
			# aggregated data
			'aggregated'   = paste0(AGG.repo,format(Sys.Date()-1,format="%m-%d-%Y"),".csv"),
			# GLOBAL TimeSeries cases
			'ts-confirmed' = paste0(TS.repo,"time_series_covid19_confirmed_global.csv"),
			'ts-deaths'    = paste0(TS.repo,"time_series_covid19_deaths_global.csv"),
			'ts-recovered' = paste0(TS.repo,"time_series_covid19_recovered_global.csv"),
			# US TimeSeries cases
                        'ts-confirmed-us' = paste0(TS.repo,"time_series_covid19_confirmed_US.csv"),
                        'ts-deaths-us'    = paste0(TS.repo,"time_series_covid19_deaths_US.csv"),
			# depricated time series
			'ts-dep-confirmed' = paste0(TS.repo,"time_series_19-covid-Confirmed.csv"),
			'ts-dep-deaths'    = paste0(TS.repo,"time_series_19-covid-Deaths.csv"),
			'ts-dep-recovered' = paste0(TS.repo,"time_series_19-covid-Recovered.csv")
		)

		# URL and filename
		cases.URL <- cases
    if (!acknowledge) {
		  message("Data being read from JHU/CCSE repository")
		  header('~')
    }
	} else {
		covid19.pckg <- 'covid19.analytics'
		if (!acknowledge) {
		  message("Data being read from *local* repo in the '",covid19.pckg,"' package")
		  header('~')
		}

		#LOCAL.repo <- "data/"
		#print(system.file("extdata", "03-27-2020.csv", package = covid19.pckg))

		cases <- switch(tolower(case),
                        # aggregated data
                        # 'aggregated'   = paste0(LOCAL.repo,format(Sys.Date()-1,format="%m-%d-%Y"),".csv"),
			# 'aggregated'   = paste0(LOCAL.repo,"03-24-2020.csv"),
			# 'aggregated'   = system.file("extdata","09-15-2020.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'aggregated'   = system.file("extdata","latest.RDS", package=covid19.pckg, mustWork = TRUE),
                        # GLOBAL TimeSeries cases
                        # 'ts-confirmed' = paste0(LOCAL.repo,"time_series_covid19_confirmed_global.csv"),
                        # 'ts-deaths'    = paste0(LOCAL.repo,"time_series_covid19_deaths_global.csv"),
			'ts-confirmed' =  system.file("extdata","time_series_covid19_confirmed_global.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'ts-deaths'    =  system.file("extdata","time_series_covid19_deaths_global.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'ts-recovered' =  system.file("extdata","time_series_covid19_recovered_global.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			# US TimeSeries cases
			'ts-confirmed-us' =  system.file("extdata","time_series_covid19_confirmed_US.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'ts-deaths-us'    =  system.file("extdata","time_series_covid19_deaths_US.csv.RDS", package=covid19.pckg, mustWork = TRUE),
                        # depricated time series
                        # 'ts-dep-confirmed' = paste0(LOCAL.repo,"time_series_19-covid-Confirmed.csv"),
                        # 'ts-dep-deaths'    = paste0(LOCAL.repo,"time_series_19-covid-Deaths.csv"),
                        # 'ts-dep-recovered' = paste0(LOCAL.repo,"time_series_19-covid-Recovered.csv")
			'ts-dep-confirmed' = system.file("extdata","time_series_19-covid-Confirmed.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'ts-dep-deaths'    = system.file("extdata","time_series_19-covid-Deaths.csv.RDS", package=covid19.pckg, mustWork = TRUE),
			'ts-dep-recovered' = system.file("extdata","time_series_19-covid-Recovered.csv.RDS", package=covid19.pckg, mustWork = TRUE)
                )

		cases.URL <- cases
  }

	if (!acknowledge) {
	  message("Reading data from ", cases)
	}

	# Attempt to protect against bad internet conenction or misspelled package name
	tryCatch( {
		if (!local.data) {
			# read data from the URL
			covid19.cases <- read.csv(cases.URL, header=TRUE)
		} else {
			# will read data from LOCAL data, using RDS
			load(cases.URL)
		}

		if (tolower(case) != 'aggregated') {
			# US cases are reported with additional fields
			if (grepl("us",tolower(case))) {
				cty.col <- pmatch("Country",names(covid19.cases))
				prov.col <- pmatch("Prov",names(covid19.cases))
				lat.col <- pmatch("Lat",names(covid19.cases))
				long.col <- pmatch("Long",names(covid19.cases))
				dates.col <-which(grepl("X",names(covid19.cases)))
				covid19.cases <- covid19.cases[,c(cty.col,prov.col,lat.col,long.col,dates.col)]
			}

			#restructure the column names for the dates
			beginning.dates <- 5
			ending.dates <- length(covid19.cases)
			names(covid19.cases)[beginning.dates:ending.dates] <- as.character(as.Date(substr(names(covid19.cases)[beginning.dates:ending.dates],2,9),format='%m.%d.%y'))

			t0 <- names(covid19.cases)[beginning.dates]
			tf <- names(covid19.cases)[ncol(covid19.cases)]

			if (!acknowledge) {
  			message("Data retrieved on ",Sys.time()," || ",
  				"Range of dates on data: ",t0,"--",tf,
  				" | Nbr of records: ",nrow(covid19.cases))
  			header('-')
			}

			# check consistency of the data
			#consistency.check(covid19.ts,datasetName=case,details=FALSE)
			#data.checks(covid19.cases)
		}

		# debriefing...
		debriefing(covid19.cases,debrief)

		# check integrity of the data
		#consistency.check(covid19.ts,dataset="combined Time Series")

		return(covid19.cases)
		},

		# warning
		warning = function(cond) {
				errorHandling.Msg(cond,cases.URL)
		},

		# error
		error = function(e){
				errorHandling.Msg(e,cases.URL)
		}
		)

	# load stored data instead
	message("We will load the preserved data instead, please notice that this data is not the latest one but instead an 'image' from previous records.")
	# will load data from local source
	return( covid19.data(case,local.data=TRUE) )
	}
                # debriefing...
                debriefing(covid19.cases,debrief)
}


###########################################################################


# wrapper function around the covid19.data() function
covid19.US.data <- function(local.data=FALSE,debrief=FALSE,acknowledge=FALSE) {
#' function to read the TimeSeries US detailed data
#'
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  TimeSeries dataframe with data for the US
#'
#' @export
#'

	# read confirmed cases
	US.conf <- covid19.data("ts-confirmed-US",local.data,acknowledge=acknowledge)

	# read deaths cases
	US.deaths <- covid19.data("ts-deaths-US",local.data,acknowledge=acknowledge)

	# combine cases
	US.cases <- rbind(US.conf,US.deaths)

        # debrief...
        debriefing(US.cases,debrief)

	return(US.cases)
}


###########################################################################
