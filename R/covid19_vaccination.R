# functions to obtain covid19 vaccination data
# SRC: Our World In Data
#
# M.Ponce - covid19.analytics 


OWID.repos <- function(tgt) {
#' function to define the OWID repos URLs
#'
#' @param  tgt  target case:  'global','us','country','locations'
#'
#' @return  URL
#'

	# OWID gtihub repo
	owid.repo <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/"
	vacc.locns <- paste0(owid.repo,"locations.csv")

	# United States vaccination data
	#
	# State-by-state data on United States COVID-19 vaccinations. We rely on the data updated daily by the United States Centers for Disease Control and Prevention. Stored in us_state_vaccinations.csv.
	vacc.us <- paste0(owid.repo,"us_state_vaccinations.csv")

	# Vaccination data
	#
	# Country-by-country data on global COVID-19 vaccinations. We only rely on figures that are verifiable based on public official sources. Stored in vaccinations.csv.
	vacc.global <- paste0(owid.repo,"vaccinations.csv")

	# specific per country -- it will be one file per country
	vacc.country <- paste0(owid.repo,"country_data/")

	if (tgt == 'locations') {
		repo <- vacc.locns
	} else if (tgt == 'us') {
		repo <- vacc.us
	} else if (tgt == 'global') {
		repo <- vacc.global
	} else if (tgt == 'country') {
		repo <- vacc.country
	} else {
		stop("Error : unrecognized target! -- ",tgt)
	}

	return(repo)
}


c19.vaccination <- function(repo, disclaimer=TRUE){
#' function to read data from OWID repo
#'
#' @param  repo  URL for reading the data
#' @param  disclaimer  indicate whether the information about the source of the data is disclosed 

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

	# select repo depending on target
	repo <- OWID.repos(tgt)

	# distinguish between 'country' and everything else
	if (tgt != 'country') {
		return(c19.vaccination(repo,disclaimer))
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
			vacc.countries[[cty]] <- c19.vaccination(paste0(repo,"/",cty,'.csv'))
		}
		return(vacc.countries)
	}
}
