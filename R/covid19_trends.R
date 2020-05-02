# Trends fns of the covid19.analytics package
#
# M.Ponce


#######################################################################

##################################################################################

itrends <- function(ts.data=NULL, geo.loc=NULL, with.totals=FALSE, fileName=NULL) {
#' function to visualize trends in daily changes in time series data interactively
#'
#' @param  ts.data  time series dataset to process
#' @param  geo.loc  geographical location, country/region or province/state to restrict the analysis to
#' @param  with.totals  a boolean flag to indicate whether the global totals should be displayed with the records for the specific location
#' @param  fileName  file where to save the HTML version of the interactive figure
#'
#' @export
#'

	# stride, "lag", when computing differences
	dlag <- 1

        ############

        agg.cases <- function(total.cases, col1,colN, geo.loc=NULL) {

                if (!is.null(geo.loc)) {
                        total.cases <- select.per.loc(total.cases,geo.loc)
                }

                confirmed <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
                recovered <- apply(total.cases[total.cases$status=="recovered",col1:colN], MARGIN=2,sum)
                deaths <- apply(total.cases[total.cases$status=="death",col1:colN], MARGIN=2,sum)
                active.cases <- confirmed - (recovered+deaths)

                return(list(confirmed=confirmed,recovered=recovered,deaths=deaths,active=active.cases))
        }


        ############

        add.traces <- function(ifig, confirmed,recovered,deaths,active.cases, style="lines", wdt=0.5,
				vis=c(TRUE,"legendonly","legendonly","legendonly","legendonly"), geo.lab="",
				dlag=1) {

                        ifig <- ifig %>% add_trace( x=log1p(confirmed[-length(confirmed)]), y = log1p(diff(confirmed, lag=dlag)),
							name=paste(geo.lab,"confirmed"),
							type='scatter', mode=style, line = list(shape = "spline", width=wdt),
							visible=vis[1] )
                        ifig <- ifig %>% add_trace( x=log1p(recovered[-length(recovered)]), y = log1p(diff(recovered,lag=dlag)),
							name=paste(geo.lab,"recovered"),
							type='scatter', mode=style, line = list(shape = "spline", width=wdt),
							visible=vis[2] )
                        ifig <- ifig %>% add_trace( x=log1p(deaths[-1]), y = log1p(diff(deaths,lag=dlag)),
							name=paste(geo.lab,"deaths"),
							type='scatter', mode=style, line = list(shape = "spline", width=wdt),
							visible=vis[3] )
                        ifig <- ifig %>% add_trace( x=log1p(active.cases[-1]), y = log1p(diff(active.cases,lag=dlag)),
							name=paste(geo.lab,"active cases"),
							type='scatter', mode=style, line = list(shape = "spline", width=wdt),
							visible=vis[4] )

                        return(ifig)
        }



        ####################################################################################

	# if not data is specified use the TS data
        if (is.null(ts.data)) {
                ts.data <- covid19.data("ts-ALL")
        }

        # check time series
        chk.TS.data(ts.data,xtp=TRUE)


	if (!is.null(geo.loc)) {
                if (length(geo.loc)>1 || toupper(geo.loc) != "ALL") {
                        # check geographical location
                        geo.loc <- checkGeoLoc(ts.data,geo.loc)
                        total.cases <- select.per.loc(ts.data,geo.loc)
                } else {
                        geo.loc <- checkGeoLoc(ts.data)
                        total.cases <- select.per.loc(ts.data,geo.loc)
                        if (!with.totals) {
                                #message("")
                                with.totals <- TRUE
                        }
                }
        } else {
                # if geo.loc0 was NULL ==> Global Totals
                geo.loc <- NULL
        }


       # remove NAs
        total.cases <- na.omit(total.cases)


        # specify range of data in TS data...
        col1 <-5; colN <- ncol(total.cases)
	cty.col <- pmatch("Country",names(total.cases))
	prv.col <- pmatch("Province",names(total.cases))

        # check whether is the whole dataset...
        if ("status" %in% tolower(colnames(total.cases))) {
                all.cases <- TRUE
                colN <- colN-1

                ###
               # categories <- unique(total.cases$status)
               # totals.per.cat <- list()
               # for (categ in seq_along(categories)) {
               #         totals.per.cat[categ] <- apply(total.cases[total.cases$status==categ,col1:colN], MARGIN=2,sum) 
               # }
                ##
                confirmed <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
                recovered <- apply(total.cases[total.cases$status=="recovered",col1:colN], MARGIN=2,sum)
                deaths <- apply(total.cases[total.cases$status=="death",col1:colN], MARGIN=2,sum)
                active.cases <- confirmed - (recovered+deaths)
                ###
        } else {
                all.cases <- FALSE
                X.cases <- apply(total.cases[,col1:colN], MARGIN=2,sum)
        }


	#plot(log1p(as.numeric(ddd[2,5:(ncol(ddd)-2)])),log1p(diff(as.numeric(ddd[2,5:(ncol(ddd)-1)]))),'b')
	#
	#abline(0,1)


              # load/check plotly
                loadLibrary("plotly")

                # define interactive figure/plot
                trends.ifig <- plot_ly(total.cases)#, x = ~x.var)
                if (all.cases) {
#                       for (categ in categories) {
#                               fig <- fig %>% add_trace(y = ~categ, name="confirmed", mode='line+markers')
#                       }
               aggs <- agg.cases(ts.data, col1,colN)
                confirmed <- aggs$confirmed
                recovered <- aggs$recovered
                deaths <- aggs$deaths
                active.cases <- aggs$active

			trends.ifig <- add.traces(trends.ifig, confirmed,recovered,deaths,active.cases, wdt=1, geo.lab="Global")
                        # extra traces for activating log-scale
                        #trends.ifig <- add.traces(trends.ifig, confirmed,recovered,deaths,active.cases, vis=FALSE,geo.lab="Global")
                        # log-scale menu based on nbr of traces...
                        updatemenues <- log.sc.setup(4)
                        nbr.log.traces <- 4
                        nbr.sets <- 1
                } else {
			my.X <- as.numeric(log1p(X.cases[-length(X.cases)]))
			my.Y <- as.numeric(log1p(diff(X.cases, lag=dlag)))
			hover.txt <- paste("Global \n",names(X.cases[-length(X.cases)])," - ",X.cases[-length(X.cases)])
                        trends.ifig <- trends.ifig %>% add_trace(x=my.X, y=my.Y,  type='scatter', mode='lines', line=list(shape = "spline", width=2),
									name="Global", hoverinfo="text", text=hover.txt )
                        # extra traces for activating log-scale
#			trends.ifig <- trends.ifig %>% add_trace(x=X.cases[-length(X.cases)], y = diff(X.cases), name=geo.loc, type='scatter', mode='lines+markers')
                        # log-scale menu based on nbr of traces...
                        #updatemenues <- log.sc.setup(1)
                        nbr.log.traces <- 1
                        nbr.sets <- 1
                }

		if (all.cases) {
			for (geo.entry in geo.loc) {
	                aggs <- agg.cases(total.cases, col1,colN, geo.entry)
        	        confirmed <- aggs$confirmed
                	recovered <- aggs$recovered
                	deaths <- aggs$deaths
                	active.cases <- aggs$active

			trends.ifig <- add.traces(trends.ifig, confirmed,recovered,deaths,active.cases, geo.lab=geo.entry)

                        #trends.ifig <- add.traces(trends.ifig, confirmed,recovered,deaths,active.cases, vis=FALSE, style="lines",geo.lab=geo.entry)
                        nbr.log.traces <- nbr.log.traces + 4
                        nbr.sets <- nbr.sets + 1
		}
		} else {
			for (i in 1:nrow(total.cases)) {
				x.values <- (total.cases[i,col1:colN])
				my.X <- log1p(as.numeric(x.values[-length(x.values)]))
				my.Y <- log1p(diff(as.numeric(x.values,lag=dlag)))
				hover.txt <- paste(paste(as.character(unlist(total.cases[i,c(cty.col,prv.col)])),collapse=" "),'\n',
							names(x.values[-length(x.values)])," - ",x.values[-length(x.values)])
				trends.ifig <- trends.ifig %>% add_trace(x=my.X, y=my.Y,  type='scatter', mode='lines',
										line=list(shape = "spline", width=.5),
										name=paste(paste(as.character(unlist(total.cases[i,c(cty.col,prv.col)])),collapse=" ")),
										hoverinfo="text", text=hover.txt )	
			}
                }

		# add line x=y
		x.values <- apply(total.cases[,col1:colN],MARGIN=2,sum)
		my.X <- log1p(as.numeric(x.values[-length(x.values)]))
		my.Y <- log1p(diff(as.numeric(x.values,lag=dlag)))
		hover.txt <- paste(names(x.values[-length(x.values)])," - ",x.values[-length(x.values)])
		trends.ifig <- trends.ifig %>% add_trace(x=my.Y, y=my.Y,  type='scatter', mode='lines',
							line=list(width=4, color = 'rgba(0,100,80,0.2)'),
							hoverinfo="text", text=hover.txt,
							showlegend = FALSE)

                # log-scale menu based on nbr of traces...
                updatemenues <- log.sc.setup(nbr.log.traces/nbr.sets,nbr.sets)

                # add a menu for switching log/linear scale
                #trends.ifig <- trends.ifig %>% layout(updatemenus=updatemenues)

                # add title
                trends.ifig <- trends.ifig %>% layout(title=paste("covid19.analytics -- Trends ",geo.loc," / ",Sys.Date()))

                # activate interactive figure
                #print(totals.ifig)


                if (!is.null(fileName)) {
                        FileName <- paste0(fileName,".html")
                        # informing where the plot is going to be saved
                        message("Saving interactive plot in ", FileName)
                        htmlwidgets::saveWidget(as_widget(trends.ifig), FileName)
                }

        print(trends.ifig)
}




##################################################################################


