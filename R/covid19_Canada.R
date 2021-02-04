
# Data acquisition fns for the "City of Toronto" and CANADA
# Part of the covid19.analytics package
#
# M.Ponce
  
#######################################################################


covid19.Toronto.data <- function(origin="OD", data.fmt="TS",local.data=FALSE,debrief=FALSE, OLD.fmt=FALSE, acknowledge=FALSE) {
#' function to import data from the city of Toronto, ON - Canada
#' as reported by the City of Toronto OR Open Data Toronto
#'
#' @param  origin  select between the "City of Toronto" ('city') OR "Open Data Toronto" ('OD')
#' @param  data.fmt  "TS" for TimeSeries of cumulative cases or "original" for the data as reported in the google-document with multiple sheets
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  OLD.fmt  boolean flag to specify if the data is being read in an old format
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe (or a list in the case of "original") with the latest data reported for the city of Toronto, ON - Canada
#'
#' @export
#'

        if (origin=="city") {
                # from the City of Toronto
                covid19.Toronto_city.data(data.fmt=data.fmt,local.data=local.data,debrief=debrief, OLD.fmt=OLD.fmt, acknowledge=acknowledge)
        } else {
                # will push all other request to Open Data Toronto
                covid19.Toronto_OD.data(data.fmt=data.fmt,local.data=local.data,debrief=debrief, acknowledge=acknowledge)
        }

}




###########################################################################


covid19.Toronto_city.data <- function(data.fmt="TS",local.data=FALSE,debrief=FALSE, OLD.fmt=FALSE, acknowledge=FALSE) {
#' function to import data from the city of Toronto, ON - Canada
#' as reported by the City of Toronto
#'	https://www.toronto.ca/home/covid-19/covid-19-latest-city-of-toronto-news/covid-19-status-of-cases-in-toronto/
#'
#' @param  data.fmt  "TS" for TimeSeries of cumulative cases or "original" for the data as reported in the google-document with multiple sheets
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  OLD.fmt  boolean flag to specify if the data is being read in an old format
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe (or a list in the case of "original") with the latest data reported for the city of Toronto, ON - Canada
#'
#' @importFrom  utils  download.file
#' @importFrom  readxl  excel_sheets read_excel
#'
#' @export
#'
	loadLibrary("readxl")

	# identify source of the data
	if (!local.data) {
		# Google drive URL, with "City of Toronto" data
		city.of.Toronto.data <- "https://drive.google.com/uc?export=download&id=1euhrML0rkV_hHF1thiA0G5vSSeZCqxHY"
		# temporary file to retrieve data, does not exist yet ==> mustwork=FALSE to avoid warning message
		Tor.xlsx.file <- normalizePath(file.path(tempdir(), "covid19-toronto.xslx"), mustWork=FALSE)
		if (!acknowledge) {
		  header('',paste("Accessing file from...",Tor.xlsx.file))
		}

		# save excel file
		#if (capabilities('libcurl')) {
		#	dwnld.method <- 'libcurl'
		#} else {
		#	stop("curl/libcurl; needed to download data from internet")
		#}
		download.file(city.of.Toronto.data, destfile=Tor.xlsx.file, mode = 'wb' )	#method=dwnld.method)
        } else {
                # use local data
  		covid19.pckg <- 'covid19.analytics'
  		if (!acknowledge) {
    		message("Data being read from *local* repo in the '",covid19.pckg,"' package")
        header('~')
  		}
      Tor.xlsx.file <- system.file("extdata","covid19_Toronto.xlsx", package=covid19.pckg, mustWork = TRUE)
    }


	if (file.exists(Tor.xlsx.file)) {
		###~~~~~~
		#print(Tor.xlsx.file)
		###~~~~~~
		# obtain names of sheets
		lst.sheets <- excel_sheets(Tor.xlsx.file)

		#print(lst.sheets)

		# if only "TS" identify corresponding sheet
		key.wrd <- "Cumulative Cases by Reported"
		tgt.sheet <- pmatch(key.wrd,lst.sheets)
		#if (is.na(tgt.sheet)) {
		#	#key.wrd <- "Cases by Episode Date"
		#	#tgt.sheet <- pmatch(key.wrd,lst.sheets)
		#	covid19.Toronto.data(data.fmt,local.data=TRUE,debrief, OLD.fmt, acknowledge)
		#}

		# read data
		if (toupper(data.fmt)=="TS") {
			# check that the target sheet was found
			if (!is.na(tgt.sheet)) {
			  if (!acknowledge) {
			    header('',"Reading TimeSeries data...")
			  }
				toronto <- read_excel(Tor.xlsx.file,sheet=tgt.sheet)
			} else {
				message(key.wrd, " NOT FOUND!")
				toronto.loc <- covid19.Toronto_city.data(data.fmt,local.data=TRUE,debrief=debrief, OLD.fmt, acknowledge=acknowledge)
				return(toronto.loc)
			}
		} else {
			  if (!acknowledge) {
			    header('',"Collecting all data reported...")
			  }
			toronto <- list()
			# iterate on each sheet...
			for (sht in lst.sheets) {
				toronto[[sht]] <- read_excel(Tor.xlsx.file,sheet=sht)
			}
		}

		# clean-up after reading the file only if it isn't the local repo
		if (!local.data) file.remove(Tor.xlsx.file)
	} else {
		if (!local.data) {
			warning("Could not access data from 'City of Toronto' source, attempting to reach local repo")
			toronto.loc <- covid19.Toronto_city.data(data.fmt=data.fmt,local.data=TRUE,debrief=debrief, OLD.fmt, acknowledge=acknowledge)
			return(toronto.loc)
		} else {
			stop("An error occurred accessing the data for the City of Toronto")
		}
	}


	if (toupper(data.fmt)=="TS") {
		## PREVIOUS FORMAT -- cases identified in 3 categories: deaths, active, resolved
		if (OLD.fmt) {
			# identify columns
			cat.col <- 2
			date.col <- 1
			nbr.col <- 3

			# filter categories
			categs <- unique(toronto[[cat.col]])
			# sort them alphabetically
			categs <- sort(categs)

			# check for inconsistencies in data, ie. missing categories
			if (length(categs) != 3) {
				stop("There supppose to be at least three categories/status within the data!\n This may represent some inconsistency with the datasets please contact the author of the package.")
			}

			# break into different categories
			data.per.categ <- split(toronto, toronto[[cat.col]])

			# Convert into TS format
			x <- data.frame()
			for (i in categs) {
				reported.dates <- rev(unique(as.Date(data.per.categ[[i]][[date.col]])))
				x <- rbind(x,rev(data.per.categ[[i]][[nbr.col]]))
			}
		###########
		} else {
		###########
			date.col <- 1
			categs <- names(toronto[2:4])

			# get the dates...
			reported.dates <- rev(unique(as.Date(toronto[,date.col][[1]])))

			x <- data.frame()
			# Convert into TS format
			for (i in categs) {
				#reported.dates <- rev(unique(as.Date(data.per.categ[[i]][[date.col]])))
				#x <- rbind(x,rev(data.per.categ[[i]][[nbr.col]]))
				data.per.categ <- toronto[,i]
				x <- rbind(x,rev(data.per.categ[[1]]))
			}
		}

		# add category
		x <- cbind(x, categs)

		## OLD WAY!!!! ###
		#reported.dates <- rev(as.Date(toronto[[date.col]]))
		#reported.cases <- rev(toronto[[nbr.col]])
		#
		#tor.data <- cbind(data.frame("Canada","Toronto, ON",43.6532,79.3832),
		#				rbind(as.integer(reported.cases)) )
		##################

		tor.data <- cbind(data.frame("Canada","Toronto, ON",43.6532,79.3832), x)

		names(tor.data) <- c("Country.Region","Province.City","Lat","Long",
					as.character(reported.dates),
					"status")
	} else {
		# ALL DATA
		tor.data <- toronto
		print(names(tor.data))
	}

	# debrief...
	debriefing(tor.data,debrief)


	return(tor.data)
}


###########################################################################


covid19.Toronto_OD.data <- function(data.fmt="TS",local.data=FALSE,debrief=FALSE, acknowledge=FALSE) {
#' function to import data from the city of Toronto, ON - Canada
#' as reported by Open Data Toronto
#'	https://open.toronto.ca/dataset/covid-19-cases-in-toronto/
#' This dataset is updated WEEKLY.
#'
#' @param  data.fmt  "TS" for TimeSeries of cumulative cases or "original" for the data as original reported
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe with the latest data reported by "OpenData Toronto" for the city of Toronto, ON - Canada
#'
#'
#' @export
#'

	# read data
	openDataTOR <- covid19.URL_csv.data(local.data, acknowledge,
				srcURL="https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv",
				srcName="Open Data Toronto",
				locFileName="covid19_openData_Toronto.RDS", locVarName="openDataTOR")

	# TS data
	if (data.fmt=="TS") {
		# identify type of cases and dates reported
		cases.types <- unique(openDataTOR$Outcome)
		cases.dates <- unique(openDataTOR$Episode.Date)

		df.cum.cases <- data.frame()
		for (i in cases.types) {
			cum.cases <- cumsum(as.numeric(tapply(openDataTOR$Outcome==i,openDataTOR$Episode.Date,sum)))
			names(cum.cases) <- sort(unique(openDataTOR$Episode.Date))
			df.cum.cases <- rbind(df.cum.cases,cum.cases)
		}

		names(df.cum.cases) <- sort(unique(openDataTOR$Episode.Date))
		df.cum.cases <- cbind(df.cum.cases, Status=as.character(cases.types), stringsAsFactors=FALSE)

		# fix names and conventions
		colnames <- names(df.cum.cases)
		# JE SUIS ICI!!!
		names(df.cum.cases)[length(colnames)] <- "status"
		colnames <- names(df.cum.cases)
		df.cum.cases[df.cum.cases$status=="FATAL","status"] <- "Deaths"
		df.cum.cases[df.cum.cases$status=="ACTIVE","status"] <- "Active Cases"
		df.cum.cases[df.cum.cases$status=="RESOLVED","status"] <- "Recovered Cases"

		tor.data <- cbind(data.frame("Canada","Toronto, ON",43.6532,79.3832), df.cum.cases)

		names(tor.data) <- c("Country.Region","Province.City","Lat","Long",
					colnames)

		# debriefing...
		debriefing(tor.data,debrief)

		return(tor.data)
	} else {
		# ORIGINAL data as reported by OpenData Toronto

		# debriefing...
		debriefing(openDataTOR,debrief)

		return(openDataTOR)
	}
}

#######################################################################

covid19.Canada.data <- function(data.fmt="TS",local.data=FALSE,debrief=FALSE, acknowledge=FALSE) {
#' function to import data for Canada
#' as reported by Health Canada
#'      https://health-infobase.canada.ca/src/data/covidLive/covid19.csv
#'
#'
#' @param  data.fmt  "TS" for TimeSeries of cumulative cases or "original" for the data as original reported
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  debrief  boolean specifying whether information about the read data is going to be displayed in screen
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#'
#' @return  a dataframe with the latest data reported by "OpenData Toronto" for the city of Toronto, ON - Canada
#'
#'
#' @export
#'

	# read data
	data <- covid19.URL_csv.data(local.data, acknowledge,
			srcURL="https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
			srcName="Health Canada",
			locFileName="covid19_HealthCanada_Canada.RDS", locVarName="canada_covid19")

	return(data)
}



#######################################################################


covid19.URL_csv.data <- function(local.data=FALSE, acknowledge=FALSE,
				srcURL="",	#"https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv",
				srcName="",	#"Open Data Toronto",
				locFileName=NA,	#"covid19_openData_Toronto.RDS",
				locVarName=NA) {	#"openDataTOR") {
#' 
#' function to read CSV from URLs or local replicas
#'
#' @param  local.data  boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity
#' @param  acknowledge  boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.
#' @param  srcURL  URL from where to obtain the data
#' @param  srcName  name of the source
#' @param  locFileName  name of the file to read from local repo
#' @param  locVarName  name of the variable loaded from local file
#'
#' @return  data as oriignally obtained from the URL src
#'
#'
#' @export
#'

        # identify source of the data
        if (!local.data) {

                # define URL to get the data
                #OpenData.Toronto.CSV <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv"
		URL.file.CSV <- srcURL

                if (!acknowledge) header('',paste0("Reading data from ",srcName," ..."))

                # read data directly from internet
                data.from.URL <- read.csv(URL.file.CSV)

		return(data.from.URL)
        } else if (!is.na(locFileName)) {
			if (!acknowledge) header('',paste0("Reading data from *LOCAL REPO* for ",srcName," ..."))
			covid19.pckg <- "covid19.analytics"
			loc.data.file <- system.file("extdata",locFileName, package=covid19.pckg, mustWork = TRUE)
			if (file.exists(loc.data.file)) {
				locVarName0 <- load(loc.data.file)
			} else {
				stop("Local data file ",loc.data.file," associated to ",locFileName," NOT found!")
			}

			if (!(locVarName %in% ls()) & !(locVarName0 %in% ls()) )
				stop("Couldn't load data from local file",loc.data.file)

			return(eval(parse(text=locVarName)))
		}

}


########################################################################
