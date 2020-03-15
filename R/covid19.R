covid19.data <- function(case='ALL', debrief=FALSE) {
#' fucntion to read lice data as reported by JHU's CCSE repo
#'
#' @param  case  a string indicating the category of the data, possible values are: "confirmed", "deaths", "recovered" OR "ALL"
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#'
#' @return  a dataframe with the daily data for the selected category per country/region/city
#'
#' @importFrom utils  read.csv
#'
#' @export
#'
#' @examples
#' covid19.all.cases <- covid19.data()
#' covid19.confirmed.cases <- covid19.data("confirmed")
#' covid19.deaths <- covid19.data("deaths")
#' covid19.recovered <- covid19.data("recovered")
#'

        ## function for error handling
        errorHandling.Msg <- function(condition,target.case) {
                message("A problem was detected when trying to retrieve the data for the package: ",target.case)
                if (grepl("404 Not Found",condition)) {
                        message("It is possible that you misspeled the name of this package! Please check!")
                } else {
                        message("It is possible that your internet connection is down! Please check!")
                }
                message(condition,'\n')

                # update problems counter
                #pkg.env$problems <- pkg.env$problems + 1
        }

        ###############################


	# URL JHU's CCSE repository
	JHU.REPO <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

	if (case == "ALL") {
		covid19.conf <- covid19.data("confirmed")
		covid19.conf <- cbind(covid19.conf, "confirmed")
		names(covid19.conf)[length(covid19.conf)] <- "status"
		covid19.death <- covid19.data("deaths")
		covid19.death <- cbind(covid19.death, "death")
		names(covid19.death)[length(covid19.death)] <- "status"
		covid19.recov <- covid19.data("recovered")
		covid19.recov <- cbind(covid19.recov, "recovered")
		names(covid19.recov)[length(covid19.recov)] <- "status"

		return(rbind(covid19.conf,covid19.death,covid19.recov))
	} else {
	# filename for corresponding cases
	cases <- switch(case,
			'confirmed' = "time_series_19-covid-Confirmed.csv",
			'deaths'    = "time_series_19-covid-Deaths.csv",
			'recovered' = "time_series_19-covid-Recovered.csv")


	# URL and filename
	cases.URL <- paste0(JHU.REPO,cases)

	cat("Reading data from ", cases,'\n')

	# Attempt to protect against bad internet conenction or misspelled package name
	tryCatch( {
		# read data from the URL
		covid19.cases <- read.csv(cases.URL, header=TRUE)

		#restructure the column names for the dates
		beginning.dates <- 5
		ending.dates <- length(covid19.cases)
		names(covid19.cases)[beginning.dates:ending.dates] <- as.character(as.Date(substr(names(covid19.cases)[beginning.dates:ending.dates],2,9),format='%m.%d.%y'))

		if (debrief) {
			print(str(covid19.cases))
			print(head(covid19.cases))
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
	}

}

###########################################################################
