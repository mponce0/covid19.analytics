covid19.data <- function(case='aggregated', local.data=FALSE, debrief=FALSE) {
#' function to read "live" data as reported by JHU's CCSE repository
#'
#' @param  case  a string indicating the category of the data, possible values are:
#'	"aggregated" :  latest number of cases *aggregated* by country,
#'	"ts-confirmed" :  time data of confirmed cases,
#'	"ts-deaths"    :  time series data of fatal cases,
#'	"ts-recovered" :  time series data of recovered cases,
#'	"ts-ALL"       :  all time series data combined,
#'	"ts-dep-confirmed" : time series data of confirmed cases as originally reported (depricated),
#'	"ts-dep-deaths"    : time series data of deaths as originally reported (depricated),
#'	"ts-dep-recovered" : time series data of recovered cases as originally reported (depricated),
#'	"ALL": all of the above
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#'
#' @return  a dataframe (or a list in the case of "ALL") with the daily worlwide indicated type of data per country/region/city
#'
#' @importFrom utils  read.csv
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
				"ts-confirmed","ts-deaths", "ts-recovered", "ts-ALL",
				"ts-dep-confirmed","ts-dep-deaths","ts-dep-recovered",
				"ALL")
	if (! tolower(case) %in% possible.cases) 
		stop("Unrecognized selection of case <",case,"> -- possible options are: ",paste(possible.cases,collapse=" "))


	if (!local.data) {
	# filename for corresponding cases
	cases <- switch(tolower(case),
			# aggregated data
			'aggregated'   = paste0(AGG.repo,format(Sys.Date()-1,format="%m-%d-%Y"),".csv"),
			# time series cases
			'ts-confirmed' = paste0(TS.repo,"time_series_covid19_confirmed_global.csv"),
			'ts-deaths'    = paste0(TS.repo,"time_series_covid19_deaths_global.csv"),
			'ts-recovered' = paste0(TS.repo,"time_series_covid19_recovered_global.csv"),
			# depricated time series
			'ts-dep-confirmed' = paste0(TS.repo,"time_series_19-covid-Confirmed.csv"),
			'ts-dep-deaths'    = paste0(TS.repo,"time_series_19-covid-Deaths.csv"),
			'ts-dep-recovered' = paste0(TS.repo,"time_series_19-covid-Recovered.csv")
		)

		# URL and filename
		cases.URL <- cases

		message("Data being read from JHU/CCSE repository")
		header('~')
	} else {
		covid19.pckg <- 'covid19.analytics'
		message("Data being read from *local* repo in the '",covid19.pckg,"' package")
		header('~')

		#LOCAL.repo <- "data/"
		#print(system.file("extdata", "03-27-2020.csv", package = covid19.pckg))

		cases <- switch(tolower(case),
                        # aggregated data
                        # 'aggregated'   = paste0(LOCAL.repo,format(Sys.Date()-1,format="%m-%d-%Y"),".csv"),
			# 'aggregated'   = paste0(LOCAL.repo,"03-24-2020.csv"),
			'aggregated'   = system.file("extdata","03-27-2020.csv", package=covid19.pckg, mustWork = TRUE),
                        # time series cases
                        # 'ts-confirmed' = paste0(LOCAL.repo,"time_series_covid19_confirmed_global.csv"),
                        # 'ts-deaths'    = paste0(LOCAL.repo,"time_series_covid19_deaths_global.csv"),
			'ts-confirmed' =  system.file("extdata","time_series_covid19_confirmed_global.csv", package=covid19.pckg, mustWork = TRUE),
			'ts-deaths'    =  system.file("extdata","time_series_covid19_deaths_global.csv", package=covid19.pckg, mustWork = TRUE),
			'ts-recovered' =  system.file("extdata","time_series_covid19_recovered_global.csv", package=covid19.pckg, mustWork = TRUE),

                        # depricated time series
                        # 'ts-dep-confirmed' = paste0(LOCAL.repo,"time_series_19-covid-Confirmed.csv"),
                        # 'ts-dep-deaths'    = paste0(LOCAL.repo,"time_series_19-covid-Deaths.csv"),
                        # 'ts-dep-recovered' = paste0(LOCAL.repo,"time_series_19-covid-Recovered.csv")
			'ts-dep-confirmed' = system.file("extdata","time_series_19-covid-Confirmed.csv", package=covid19.pckg, mustWork = TRUE),
			'ts-dep-deaths'    = system.file("extdata","time_series_19-covid-Deaths.csv", package=covid19.pckg, mustWork = TRUE),
			'ts-dep-recovered' = system.file("extdata","time_series_19-covid-Recovered.csv", package=covid19.pckg, mustWork = TRUE)
                )

		cases.URL <- cases
        }

	message("Reading data from ", cases)

	# Attempt to protect against bad internet conenction or misspelled package name
	tryCatch( {
		# read data from the URL
		covid19.cases <- read.csv(cases.URL, header=TRUE)

		if (tolower(case) != 'aggregated') {
			#restructure the column names for the dates
			beginning.dates <- 5
			ending.dates <- length(covid19.cases)
			names(covid19.cases)[beginning.dates:ending.dates] <- as.character(as.Date(substr(names(covid19.cases)[beginning.dates:ending.dates],2,9),format='%m.%d.%y'))

			t0 <- names(covid19.cases)[5]
			tf <- names(covid19.cases)[ncol(covid19.cases)]

			message("Data retrieved on ",Sys.time()," || ",
				"Range of dates on data: ",t0,"--",tf,
				" | Nbr of records: ",nrow(covid19.cases))
			header('-')
		}

		if (debrief) {
			print(str(covid19.cases))
			print(head(covid19.cases))
			print(names(covid19.cases))
		}

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

}

###########################################################################
