# module for computing rolling estimates for fatality and recovery rates
# part of covid19.analytics package
#
# M.Ponce

#########

rollingRate <- function(data, fn=mean, period=NULL) {
#' function to compute a rolling fn of a TS data
#'
#' @param  data  TS data
#' @param  fn  function to compute rolling
#' @param  period  length of window
#'
#' @return a vector with rolling values
#'
#' @export
#'

	ini.col <- 5
	lst.col <- ncol(data)

	X <- unlist(data[ini.col:lst.col])

	if (is.null(period))
		period <- length(X)

	Y <- movingFn(X,fn=fn,period=period)

	names(Y) <- names(data[ini.col:lst.col])

	return(Y)
}

#########

mrollingRates <- function(data=NULL, geo.loc=NULL, fn=mean, period) {
#' function to compute a rolling fn (rate) of multiple quantities from TS data, eg. fatality and recovery rate
#'
#' @param  data  time series dataset to consider
#' @param  geo.loc  country/region to analyze
#' @param  fn  function to compute rolling
#' @param  period  length of window
#'
#' @export
#'

        if (is.null(data) | is.null(geo.loc))
                stop("Arguments needed!")

        # check that the data is time series data
        chk.TS.data(data,xtp=TRUE)

        # check on the location
        geo.loc <- checkGeoLoc(data,geo.loc)

        cases.per.loc <- select.per.loc(data,geo.loc)

        if ("status" %in% names(cases.per.loc)) {
                sts <- unique(cases.per.loc$status)
		rolls <- data.frame()
		for (j in sts) {
			rolls <- rbind(rolls, rollingRate(cases.per.loc[cases.per.loc$status==j,1:ncol(cases.per.loc)-1], fn=fn, period=period))
		}
		rownames(rolls) <- sts
		names(rolls) <- names(cases.per.loc)[5:(ncol(cases.per.loc)-1)]
	} else {
		rolls <- rollingRate(cases.per.loc, fn=fn, period=period)
	}

	return(rolls)
}

#########

estimateRRs <- function(data=NULL, geo.loc=NULL, period=NULL, graphics.ON=TRUE, splitG=TRUE) {
#' estimate rolling rates for a given geographical location for an specific TS data
#'
#' @param  data  time series dataset to consider
#' @param  geo.loc  country/region to analyze
#' @param  period  length of window
#' @param  graphics.ON  boolean flag to activate/deactivate graphical output
#' @param  splitG  boolean flag for having the graphical output separated or not
#'
#' @export
#'
#' @examples
#' # the following examples take longer than 10 sec, and triggers CRAN checks
#' \dontrun{
#' estimateRRs(covid19.data("TS-all"), geo.loc='Peru', period=7)
#' estimateRRs(covid19.data("TS-all"),
#'		geo.loc=c('Peru','Argentina','Uruguay','US','Spain','Japan'), period=7)
#' }
#'
	if (length(geo.loc) > 1) {
		rtn <- list()
		for (locn in geo.loc) {
			rtn <- list(rtn,c(locn,estimateRRs(data, geo.loc=locn, period=period, graphics.ON=graphics.ON, splitG=splitG)))
		}
		#rtn <- rtn[[1]][[-1]]
	} else {


	# determine rolling mean using the window *period*
	rrs.m <- mrollingRates(data, geo.loc, fn=mean, period=period)
	fat.rate <- as.numeric(rrs.m[2,]/rrs.m[1,])*100
	rec.rate <- as.numeric(rrs.m[3,]/rrs.m[1,])*100

	# rolling rate using period=NULL => usual mean value
	cnst.m <- mrollingRates(data, geo.loc, fn=mean, period=NULL)
	cnst.fat.rate <- as.numeric(cnst.m[2,]/cnst.m[1,])*100
	cnst.rec.rate <- as.numeric(cnst.m[3,]/cnst.m[1,])*100

	# derermine rolling sd using the window-period
	rrs.sd <- mrollingRates(data, geo.loc, fn=sd, period=period)
	fat.sd <- as.numeric(rrs.sd[2,]/rrs.sd[1,])
	rec.sd <- as.numeric(rrs.sd[3,]/rrs.sd[1,])

#print(rrs.m)

	if (graphics.ON) {
        ### preserve user graphical env.
        # save the state of par() before running the code
        oldpar <- par(no.readonly = TRUE)
        # restore the previous state after the fn is done, even if it fails, so the user environment is not altered
        on.exit(par(oldpar))
        #########

		if (splitG) par(mfrow=c(2,1))
		minY <- min(c(fat.rate,rec.rate), na.rm=TRUE)
		maxY <- max(c(fat.rate,rec.rate), na.rm=TRUE)
		xrange <- 1:length(rrs.m)
		plot(xrange,fat.rate, type='b', pch=20, col='red', axes=FALSE)	#, ylim=c(minY,maxY))
		axis.Date(1,names(rrs.m)); axis(2)
		confBand(xrange,fat.rate, 1,length(fat.rate),0,max(fat.rate,na.rm=TRUE), windowsNbr=10, lcolour='red')
		lines(xrange,cnst.fat.rate, lty='dashed', col='red')

		title(main=geo.loc)
		if (!splitG) par(new=TRUE)
		plot(xrange,rec.rate, type='b', pch=20, col='blue', axes=FALSE)	#, ylim=c(minY,maxY))
		axis.Date(1,names(rrs.m)); axis(4)
		confBand(xrange,rec.rate, 1,length(fat.rate),0,max(fat.rate,na.rm=TRUE), windowsNbr=10, lcolour='blue')
		lines(xrange,cnst.rec.rate, lty='dashed', col='blue')
	}

	# combine results into returning object
	rtn <- list(RollingRateEstimates=rrs.m,
			FatalityRate=fat.rate,
			RecoveryRate=rec.rate)
	}


	return(rtn)
}

#########
