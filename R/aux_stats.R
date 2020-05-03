# Auxiliary Statistical fns of the covid19.analytics package
# 
# M.Ponce


#######################################################################

movingFn <- function(x, fn=mean, period=length(x), direction="forward") {
#' generic fn that computes the "fn" on a moving window
#' @param  x  a numeric vector
#' @param  fn  a function to be applied/computed, default is set to mean()
#' @param  period  size of the "moving window", default set to the lenght of the vector
#' @param  direction  type of moving avergage to consider: "forward", "centered", "backward"; ie. whether the window computation is ( "centered" / "forward" / "backward" ) wrt the data series
#'
#' @return  a vector with the 'moving operation' applied to the x vector
#'

        if (!is.numeric(x)) stop("argument x must be of type numeric!")
        if (!is.function(fn)) stop("fn must be a function!")
        #print(period)
        if (!is.numeric(period)) stop("Argument period must be of type numeric!")

        mavg <- c()
        n <- length(x)

        #print(period) 

        if ( (period==0) || (period>=length(x)) ) {
                mavg <- rep(fn(x),length(x))
        } else {
                #print("using",as.character(fn))
                for (i in 1:length(x)) {
                        irange <- i:min(i+(period-1),n)
                        #cat(irange)
                        #print(x[irange])
                        #cat("...", fn(x[irange],na.rm=TRUE),'\n')
                        mavg <- c(mavg,fn(x[irange],na.rm=TRUE))
                }
        }

        return(mavg)
}


#######################################################################

confBand <- function(x,y, x0,x1,y0,y1, windowsNbr=10, period=ceiling(length(y)/windowsNbr), lcolour='gray',ltype=4,lwidth=2, filling=TRUE) {
#' function to draw confidence bands, using generalized moving averages/sds
#'
#' importFrom  grDevices  rgb
#' importFrom  graphics  lines polygon
#' importFrom  stats  sd
#'
#' @keywords internal
#'

                lineWrapper <- function(x,y, x0,x1,y0,y1, line.col,line.lt,line.wdt) {
                # wrapper function to draw lines

                        graphics::lines(x,y, col=line.col, lty=line.lt, lwd=line.wdt,
                                xlim=c(x0,x1), ylim=c(y0,y1), ann=FALSE)
                }

                ym <- movingFn(y,mean,period)
                ysd <- movingFn(y,stats::sd,period)

                lineWrapper(x,ym, x0,x1,y0,y1, lcolour,ltype,lwidth)
                lineWrapper(x,ym+(ysd/2), x0,x1,y0,y1, lcolour,ltype+1,lwidth/2)
                lineWrapper(x,ym-(ysd/2), x0,x1,y0,y1, lcolour,ltype+1,lwidth/2)

                # shading of the confidence region
                if (filling){
                        xprime <- c(x,rev(x))
                        yprime <- c((ym+(ysd/2)),rev(ym-(ysd/2)))

			yprime.NAs <- sum(is.na(yprime))

			#print(yprime.NAs); print(yprime)
			#print(which(is.na(yprime)))
			#print(min(which(is.na(yprime))-0))

			# check that there is actual numerical data available... othewise skip it...
			if (yprime.NAs < length(yprime)) {
				if(yprime.NAs > 0)
					yprime[which(is.na(yprime))] <- yprime[min(which(!is.na(yprime))-0)]

				graphics::polygon(xprime,yprime, col=grDevices::rgb(0.5,0.5,0.5, .25), border=NA)
			}
		}

		return(invisible(ym)) 
       }

#######################################################################

