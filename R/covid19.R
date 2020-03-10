covid19 <- function(case='ALL') {

	# URL JHU's CCSE repository
	JHU.REPO <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

	if (case == "ALL") {
		covid19.conf <- covid19("confirmed")
		covid19.conf <- cbind(covid19.conf, "confirmed")
		names(covid19.conf)[length(covid19.conf)] <- "status"
		covid19.death <- covid19("deaths")
		covid19.death <- cbind(covid19.death, "death")
		names(covid19.death)[length(covid19.death)] <- "status"
		covid19.recov <- covid19("recovered")
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

	# read data from the URL
	covid19.cases <- read.csv(cases.URL, header=TRUE)

	#restructure the column names for the dates
	beginning.dates <- 5
	ending.dates <- length(covid19.cases)
	names(covid19.cases)[beginning.dates:ending.dates] <- as.character(as.Date(substr(names(covid19.cases)[beginning.dates:ending.dates],2,9),format='%m.%d.%y'))

	print(str(covid19.cases))
	print(head(covid19.cases))

	return(covid19.cases)
	}
}
