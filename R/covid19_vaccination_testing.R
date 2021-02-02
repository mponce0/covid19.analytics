# functions to obtain covid19 Vaccination & Testing data
# SRC: "Our World In Data" (OWID)
#
# M.Ponce - covid19.analytics 


OWID.repos <- function(tgt) {
#' function to define the OWID repos URLs
#'
#' @param  tgt  target case:  'VAC.global','VAC.us','VAC.country','VAC.locations', 'testing', 'testing.details'
#'
#' @return  URL
#'

	# OWID gtihub repo
	#owid.repo <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/"
	owid.repo <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/"
	vacc.repo <- paste0(owid.repo,"vaccinations/")
	vacc.locns <- paste0(vacc.repo,"locations.csv")
	testing.repo <-  paste0(owid.repo,"testing/")
	testing.data <- paste0(testing.repo, "covid-testing-all-observations.csv")
	testing.data.details <- paste0(testing.repo,"covid-testing-latest-data-source-details.csv")

	# United States vaccination data
	#
	# State-by-state data on United States COVID-19 vaccinations. We rely on the data updated daily by the United States Centers for Disease Control and Prevention. Stored in us_state_vaccinations.csv.
	vacc.us <- paste0(vacc.repo,"us_state_vaccinations.csv")

	# Vaccination data
	#
	# Country-by-country data on global COVID-19 vaccinations. We only rely on figures that are verifiable based on public official sources. Stored in vaccinations.csv.
	vacc.global <- paste0(vacc.repo,"vaccinations.csv")

	# specific per country -- it will be one file per country
	vacc.country <- paste0(vacc.repo,"country_data/")

	# VACINATION REPOS
	if (tgt == 'VAC.locations') {
		repo <- vacc.locns
	} else if (tgt == 'VAC.us') {
		repo <- vacc.us
	} else if (tgt == 'VAC.global') {
		repo <- vacc.global
	} else if (tgt == 'VAC.country') {
		repo <- vacc.country
	# TESTING REPOS
	} else if (tgt == 'testing') {
		repo <- testing.data
	} else if (tgt == 'testing.details') {
		repo <- testing.data.details
	# UNRECOGNIZED option
	} else {
		stop("Error : unrecognized target! -- ",tgt)
	}

	return(repo)
}


c19.OWID.data <- function(repo, disclaimer=TRUE){
#' function to read data from OWID repos
#'
#' @param  repo  URL for reading the data
#' @param  disclaimer  indicate whether the information about the source of the data is disclosed 
#'
#' @return  datafram object
#'

	vacc.data <- read.csv(repo)

	if (disclaimer) {
		message("Data obtained from OWID repo:",'\n',repo)
	}

	return(vacc.data)
}



covid19.vaccination <- function(tgt="global", data.fmt='orig', disclaimer=TRUE){
#' function to read data related to covid19 vaccinations
#'
#' @param  tgt  selects data type: 'global','us','country','locations'
#' @param  data.fmt  selects the format of the data, options are: 'orig' (original as reported by the OWID repo)
#' @param  disclaimer  indicates whether the information about the source of the data is disclosed
#'
#' @export
#'

	# select repo depending on target
	tgt <- paste0("VAC.",tgt)
	repo <- OWID.repos(tgt)

	# distinguish between 'country' and everything else
	if (tgt != 'country') {
		return(c19.OWID.data(repo,disclaimer))
	} else {
		skip <- c("European Union","World")
		# obtain list of countries
		X <- covid19.vaccination("global",disclaimer=FALSE)
		ctries <- unique(X$location)
		# remove 'skipy' ones, eg. EU, ...
		ctries <- ctries[!ctries %in% skip]
		# initialize empty list
		vacc.countries <- list()
		for (cty in ctries) {
			# convert names into URL notation, ie. spaces -> %20
			cty <- gsub("[[:space:]]", "%20", cty)
			vacc.countries[[cty]] <- c19.OWID.data(paste0(repo,"/",cty,'.csv'))
		}
		return(vacc.countries)
	}
}



covid19.testing.data <- function(tgt='testing', disclaimer=TRUE){
#' function to read data related to covid19 Testing from OWID repo
#'
#' @param  tgt  selects between time series data ('testing') and details and overall view of the data ("testing.details")
#' @param  disclaimer  indicates whether the information about the source of the data is disclosed
#'
#' @return  dataframe containing list of countries (Entity) and testing data
#'
#' @export
#'

	valid.options <- c("testing","testing.details")

	if (sum(tgt %in% valid.options) != 1)
		stop("tgt argument must be one of the following options:  ", paste(valid.options,collapse=' '))

	repo <- OWID.repos(tgt)

	tst.data <- c19.OWID.data(repo,disclaimer)

	return(tst.data)
}
