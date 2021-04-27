# "covid19.analytics"  visualization and graphical functions
#
# M.Ponce

##################################################################

##################################################################
##### AUXILIARY FUNCTIONS FOR SETTING UP LOG-scale PULLDOWNS #####
##################################################################

	log.sc.setup <- function(nbr.traces,tot.traces) {
	#' aux fn to create a log-scale pulldown option in plotly plots
	#' @param  nbr.traces  number of traces in order to set Ts/Fs
	#' @keywords internal


		#print(nbr.traces)
		#print(tot.traces)
		settings <- rep(c(rep(T,nbr.traces),rep(F,nbr.traces)),tot.traces)
		#settings <- c(T,T,T,T, F,F,F,F, T,T,T,T, F,F,F,F, T,T,T,T, F,F,F,F)
		#print(settings)

                updatemenus = list(list(
                                active = 0,
                                buttons= list(
                                                list(label = 'linear',

                                                        method = 'update',
                                                        args = list(	list(visible = settings),
									list(yaxis = list(type = 'linear'))
								)
                                                        ),
                                                list(label = 'log',
                                                        method = 'update',
                                                        #args = list(list(visible = c(rep(F,nbr.traces),rep(T,nbr.traces))), list(yaxis = list(type = 'log'))))
							args = list(	list(visible = !settings),
									list(yaxis = list(type = 'log')) 
										#xaxis=list(type='log'))
								)
							)
						)
					)
				)

                return(updatemenus)
        }

        ############

        ############

        add.Ntraces <- function(i.fig, traces, tr.names=rep("",length(traces)), vis=TRUE) {

			nn <- names(traces)
			#print(nn)
			range <- 2:length(traces)
			#print(range)
			for (tr in range) {
				tr.i <- traces[nn[tr]][,1]
				#print(tr.i)
				i.fig <- i.fig %>% add_trace(x=~tr[1][,1], y = ~tr.i, name=tr.names[tr], type='scatter', mode='lines+markers', visible=vis)
			}

			return(i.fig)
	}


        add.N.traces <- function(i.fig, traces, tr.names=rep("",length(traces)), vis=TRUE) {

			tr.x <- traces[,2]
                        i.fig <- i.fig %>% add_trace(x=~tr.x, y = ~traces[,3], name=tr.names[3-1], type='scatter', mode='lines+markers', visible=vis)
			i.fig <- i.fig %>% add_trace(x=~tr.x, y = ~traces[,4], name=tr.names[4-1], type='scatter', mode='lines+markers', visible=vis)
			i.fig <- i.fig %>% add_trace(x=~tr.x, y = ~traces[,5], name=tr.names[5-1], type='scatter', mode='lines+markers', visible=vis)
			i.fig <- i.fig %>% add_trace(x=~tr.x, y = ~traces[,1], name=tr.names[2-1], type='scatter', mode='markers', visible=vis)
			# for adding the FORCE...
			i.fig <- i.fig %>% add_trace(x=~tr.x, y = ~traces[,6], name=tr.names[6-1], type='scatter', mode='lines', visible=vis)

			return(i.fig)
        }

        ############

##################################################################



totals.plt <- function(data0=NULL, geo.loc0=NULL,
			one.plt.per.page=FALSE, log.plt=TRUE, with.totals=FALSE,
			interactive.fig=TRUE, fileName=NULL, interactive.display=TRUE) {
#' function to plot total number of cases per day for different groups
#'
#' @param  data0  time series dataset to process, default all the possible cases: 'confirmed' and 'deaths' for all countries/regions
#' @param  one.plt.per.page  boolean flag to have one plot per figure
#' @param  geo.loc0  geographical location, country/region or province/state to restrict the analysis to
#' @param  log.plt  include a log scale plot in the static plot
#' @param  with.totals  a boolean flag to indicate whether the totals should be displayed with the records for the specific location
#' @param  interactive.fig  switch to turn off/on an interactive plot
#' @param  fileName  file where to save the HTML version of the interactive figure
#' @param  interactive.display  boolean argument for enabling or not displaying the interactive figure
#'
#' @export
#'
#' @importFrom  stats  na.omit
#' @importFrom  plotly  plot_ly %>% add_trace as_widget
#' @importFrom  htmlwidgets  saveWidget 
#'
#' @examples
#' # retrieve time series data
#' TS.data <- covid19.data("ts-ALL")
#'
#' # static and interactive plot 
#' totals.plt(TS.data)
#'

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

	process.mult.cases <- function(total.cases, geo.loc, col1,colN, ymax=NA, with.totals,stroke=FALSE,new.plt=FALSE) {


		if (length(geo.loc)>=1) {
			#set.plt.canvas(geo.loc,ylayers=1,minBreaks=5)
			if (!with.totals) {
				if (is.na(ymax))
					#ymax <- max(total.cases[,col1:colN],na.rm=TRUE)*1.5
					ymax <- max(apply(total.cases[total.cases$status=="confirmed",col1:colN],MARGIN=2,sum),na.rm=TRUE)
				legs <- TRUE
			} else {
				legs <- FALSE
			}
			par(new=TRUE)
			if (new.plt) par(new=FALSE)
			col.index <- 0
			if (length(geo.loc) > 10) {
				plt.title <- "Several entries..."
			} else {
				plt.title <- paste(geo.loc,collapse=' ')
			}
			for (geo.entry in geo.loc){
				geoloc.cases <- select.per.loc(total.cases,geo.entry)
				loc.confirmed <- apply(geoloc.cases[geoloc.cases$status=="confirmed" ,col1:colN], MARGIN=2,sum)
				loc.recovered <- apply(geoloc.cases[geoloc.cases$status=="recovered",col1:colN], MARGIN=2,sum)
				loc.deaths <- apply(geoloc.cases[geoloc.cases$status=="death",col1:colN], MARGIN=2,sum)
				loc.active.cases <- loc.confirmed - (loc.recovered+loc.deaths)
				if (with.totals) {
					par(new=TRUE)
				} else {
					stroke <- FALSE
				}
				static.plt(x.dates,loc.confirmed,loc.recovered,loc.deaths,loc.active.cases, ymax=ymax, geo.loc=geo.entry, legs = legs,stroke=stroke,plt.title=plt.title, col.offset=col.index)
				par(new=TRUE)
				col.index <- col.index+1
			}
		}
	}

	############

	static.plt <- function(x.dates,confirmed=NULL,recovered=NULL,deaths=NULL,active.cases=NULL,
				ymin=NA,ymax=NA, geo.loc=NULL, legs=FALSE, stroke=TRUE, plt.title='',
				col.offset=0) {
	#'
	#' @importFrom  grDevices  rgb


		if (is.na(ymax))
			ymax <- max(c(confirmed,recovered,deaths,active.cases),na.rm=TRUE)
		if (is.na(ymin))
			ymin <- 0

		#if (is.null(geo.loc) | toupper(geo.loc)!="ALL" | length(geo.loc) > 1) {
		if (!stroke) {
			perc <- 65
			alpha.level <- (100 - perc) * 255 / 100
			lws <- 1
			plt.type <- 'b'
		} else {
			alpha.level <- 255
			lws <- .5
			plt.type <- 'l'
		}

		#if (is.null(geo.loc)) {
		#	plt.title <- ''	#'global'
		#} else {
		#	plt.title <- paste(geo.loc, collapse=" ")
		#}


		if (col.offset==0) {
		colors <- c(rgb(0,0,0, maxColorValue=255, alpha=alpha.level),
				rgb(0,0,255, maxColorValue=255, alpha=alpha.level),
				rgb(255,0,0, maxColorValue=255, alpha=alpha.level),
				rgb(255,165,0, maxColorValue=255, alpha=alpha.level))
		} else {
		#colors <- c("black","red","blue","orange")
			colors <- ((4*col.offset)+1):(4*(col.offset+1))
		}
                #par(new=TRUE,ann=FALSE)
                if (!is.null(confirmed))
			plot(x.dates,confirmed, ylim=c(0,ymax),
				type=plt.type, col=colors[1], cex=0.5, pch=20, lwd=lws,
				xlab='time', ylab='nbr of cases', main=plt.title)

		if (!is.null(recovered)) {
			par(new=TRUE,ann=FALSE)
			plot(x.dates[1:length(recovered)],recovered,
				cex=0.5, pch=20, lwd=lws,
				ylim=c(0,ymax), axes=FALSE, type=plt.type, col=colors[2])
		}

		if (!is.null(deaths)) {
			par(new=TRUE,ann=FALSE)
			plot(x.dates,deaths,
				cex=0.5, pch=20, lwd=lws,
				ylim=c(0,ymax), axes=FALSE, type=plt.type, col=colors[3])
		}

		if (!is.null(active.cases)) {
			par(new=TRUE,ann=FALSE)
			plot(x.dates,active.cases,
				cex=0.5, pch=20, lwd=lws,
				ylim=c(0,ymax), axes=FALSE, type=plt.type, col=colors[4])
		}

		# add legends
		if (legs)
                legend("topleft", legend=c("Confirmed", "Recovered","Deaths","Active"),
                        col=c("black","blue","red","orange"), lty=1, lwd=2, pch=c(20,20,20,NA), cex=0.8, box.lty=0)

		title(plt.title)
		return(max(c(confirmed,recovered,deaths,active.cases), na.rm=TRUE))
	}


	############

        add.traces <- function(totals.ifig, confirmed,recovered,deats,active,cases, style="lines+markers", vis=TRUE, geo.lab="") {
                        totals.ifig <- totals.ifig %>% add_trace(y = confirmed, name=paste(geo.lab,"confirmed"), type='scatter', mode=style, visible=vis)
                        totals.ifig <- totals.ifig %>% add_trace(y = recovered, name=paste(geo.lab,"recovered"), type='scatter', mode=style, visible=vis)
                        totals.ifig <- totals.ifig %>% add_trace(y = deaths, name=paste(geo.lab,"deaths"), type='scatter', mode=style, visible=vis)
                        totals.ifig <- totals.ifig %>% add_trace(y = active.cases, name=paste(geo.lab,"active cases"), type='scatter', mode=style, visible=vis)

                        return(totals.ifig)
        }


	####################################################################################


	if (is.null(data0)) {
		data <- covid19.data("ts-ALL")
	} else {
		data <- data0
	}
	total.cases <- data

	# check time series
	chk.TS.data(total.cases,xtp=TRUE)

	if (!is.null(geo.loc0)) {
		if (toupper(geo.loc0) != "ALL") {
		# attempt to have several locations processed at once...
		#if (length(geo.loc) > 1 ) {
		#	for (geo.entry in geo.loc) {
		#		header('',paste("Processing ",geo.entry," ..."))
		#		totals.plt(data0,geo.entry,log.plt,interactive.fig,fileName)
		#	}
		#	return()
		#} else {
			# check geographical location
			geo.loc <- checkGeoLoc(total.cases,geo.loc0)
			total.cases <- select.per.loc(total.cases,geo.loc)
		#}
		} else {
			geo.loc <- checkGeoLoc(total.cases)
                        total.cases <- select.per.loc(total.cases,geo.loc) 
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
	# check whether is the whole dataset...
	if ("status" %in% tolower(colnames(total.cases))) {
		all.cases <- TRUE
		colN <- colN-1

		###
		categories <- unique(total.cases$status)
		totals.per.cat <- data.frame()
		for (categ in seq_along(categories)) {
			totals.per.cat <- c(totals.per.cat, c(categories[categ],apply(total.cases[total.cases$status==categ,col1:colN], MARGIN=2,sum) )  )
		}
		##
		#confirmed <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
		#recovered <- apply(total.cases[total.cases$status=="recovered",col1:colN], MARGIN=2,sum)
		#deaths <- apply(total.cases[total.cases$status=="death",col1:colN], MARGIN=2,sum)
		#active.cases <- confirmed - (recovered+deaths)
		#aggs <- agg.cases(total.cases, col1,colN)
		aggs <- agg.cases(data, col1,colN)
		confirmed <- aggs$confirmed
		recovered <- aggs$recovered
		deaths <- aggs$deaths
		active.cases <- aggs$active
		###
	} else {
		all.cases <- FALSE
		#X.cases <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
		X.cases <- apply(total.cases[,col1:colN], MARGIN=2,sum)
	}

	# totals per column
	#totals <- apply(total.cases[,col1:colN], MARGIN=2,sum)

	x.dates <- as.Date(names(total.cases)[col1:colN])
	#ymax <- max(totals,na.rm=TRUE)
	ymax <- NA

        ### preserve user graphical env.
        # save the state of par() before running the code
        oldpar <- par(no.readonly = TRUE)
        # restore the previous state after the fn is done, even if it fails, so the user environment is not altered
        on.exit(par(oldpar))
        #########

	### STATIC PLOTS
	#plot(x.dates, totals, ylim=c(0,ymax),  type='l', lwd=3, col='darkred',
	#	xlab='time', ylab='nbr of cases', main=geo.loc)

	if (all.cases) {
		# set layout of plots
		if (log.plt) par(mfrow=c(1,2))
		if (one.plt.per.page) par(mfrow=c(1,1))

		# linear scale plot

if ( is.null(geo.loc) || ((length(geo.loc) >=1 & with.totals) | (toupper(geo.loc0)=="ALL")) ) {
		ymax <- static.plt(x.dates,confirmed,recovered,deaths,active.cases, legs=TRUE, stroke=FALSE)#, geo.loc=geo.loc0)

		# add legends
		#legend("topleft", legend=c("Confirmed", "Recovered","Deaths","Active"),
		#	col=c("black","blue","red","orange"), lty=1, lwd=2, pch=c(20,20,20,NA), cex=0.8, box.lty=0)
n.plt <- FALSE
} else { n.plt <- TRUE }

		process.mult.cases(total.cases, geo.loc, col1,colN, ymax, with.totals,stroke=TRUE,new.plt=n.plt)

		# log-scale plot
		if (log.plt) {
			par(new=FALSE)
#plot(x.dates,log1p(confirmed),type='n')
#plot(0,0,'n',ann=FALSE)
			if ( is.null(geo.loc) || ((length(geo.loc) >=1 & with.totals) | toupper(geo.loc) == "ALL") ) {
			ymax <- static.plt(x.dates,log1p(confirmed),log1p(recovered),log1p(deaths),log1p(active.cases),stroke=FALSE)
n.plt <- FALSE
} else { n.plt <- TRUE }

			log.total.cases <- total.cases
			log.total.cases[,col1:colN] <- log1p(total.cases[,col1:colN])
			process.mult.cases(log.total.cases, geo.loc, col1,colN, ymax, with.totals,stroke=TRUE,new.plt=n.plt)
		}
	} else {
		plot(x.dates, X.cases, ylim=c(0,max(X.cases)),  type='l', lwd=3, col='darkred',
			xlab='time', ylab='nbr of cases', main=geo.loc)
	}

	### INTERACTIVE PLOTS
	if (interactive.fig) { 
		# load/check plotly
		loadLibrary("plotly")

		# define interactive figure/plot
###		totals.ifig <- plot_ly(total.cases, x = ~x.dates)	#colnames(total.cases[,col1:colN]))	#, type='scatter', mode='line+markers')
		totals.ifig <- plot_ly(data, x = ~x.dates)
		if (all.cases) {
#			for (categ in categories) {
#				fig <- fig %>% add_trace(y = ~categ, name="confirmed", mode='line+markers')
#			}
               aggs <- agg.cases(data, col1,colN)
                confirmed <- aggs$confirmed
                recovered <- aggs$recovered
                deaths <- aggs$deaths
                active.cases <- aggs$active

			totals.ifig <- totals.ifig %>% add_trace(y = confirmed, name="Global confirmed", type='scatter', mode='lines+markers')
			totals.ifig <- totals.ifig %>% add_trace(y = recovered, name="Global recovered", type='scatter', mode='lines+markers')
			totals.ifig <- totals.ifig %>% add_trace(y = deaths, name="Global deaths", type='scatter', mode='lines+markers')
			totals.ifig <- totals.ifig %>% add_trace(y = active.cases, name="Global active cases", type='scatter', mode='lines+markers')
			# extra traces for activating log-scale
			totals.ifig <- add.traces(totals.ifig, confirmed,recovered,deaths,active.cases, vis=FALSE,geo.lab="Global")
			# log-scale menu based on nbr of traces...
			#updatemenues <- log.sc.setup(4)
			nbr.log.traces <- 4
			nbr.sets <- 1
		} else {
			totals.ifig <- totals.ifig %>% add_trace(y = ~X.cases, name=geo.loc, type='scatter', mode='lines+markers')
			# extra traces for activating log-scale
			totals.ifig <- totals.ifig %>% add_trace(y = ~X.cases, name=geo.loc, type='scatter', mode='lines+markers', visible=F)
			# log-scale menu based on nbr of traces...
			#updatemenues <- log.sc.setup(1)
			nbr.log.traces <- 1
			nbr.sets <- 1
		}

		for (geo.entry in geo.loc) {
                aggs <- agg.cases(data, col1,colN, geo.entry)
                confirmed <- aggs$confirmed
                recovered <- aggs$recovered
                deaths <- aggs$deaths
                active.cases <- aggs$active
                        totals.ifig <- totals.ifig %>% add_trace(y = confirmed, name=paste(geo.entry,"confirmed"), type='scatter', mode="lines", line=list(width=1))
                        totals.ifig <- totals.ifig %>% add_trace(y = recovered, name=paste(geo.entry,"recovered"), type='scatter', mode="lines", line=list(width=1))
                        totals.ifig <- totals.ifig %>% add_trace(y = deaths, name=paste(geo.entry,"deaths"), type='scatter', mode="lines", line=list(width=1))
                        totals.ifig <- totals.ifig %>% add_trace(y = active.cases, name=paste(geo.entry,"active cases"), type='scatter', mode="line", line=list(width=1))

			totals.ifig <- add.traces(totals.ifig, confirmed,recovered,deaths,active.cases, vis=FALSE, style="lines",geo.lab=geo.entry)
			nbr.log.traces <- nbr.log.traces + 4
			nbr.sets <- nbr.sets + 1
		}

		# log-scale menu based on nbr of traces...
		updatemenues <- log.sc.setup(nbr.log.traces/nbr.sets,nbr.sets)

		# add a menu for switching log/linear scale
		totals.ifig <- totals.ifig %>% layout(updatemenus=updatemenues)

		# add title
		totals.ifig <- totals.ifig %>% layout(title=paste("covid19.analytics -- Totals ",geo.loc," / ",Sys.Date()))

		# activate interactive figure
		#print(totals.ifig)

        
		if (!is.null(fileName)) {
			FileName <- paste0(fileName,".html")
			# informing where the plot is going to be saved
			message("Saving interactive plot in ", FileName)
			htmlwidgets::saveWidget(as_widget(totals.ifig), FileName)
		}
	}

	if (interactive.display) {
		# show interactive plot
		print(totals.ifig)
		# and return DF
		return(invisible(totals.per.cat))
	} else {
		# return interactive plot
		return(totals.ifig)
	}
}



##################################################################################

live.map <- function(data=covid19.data(),
			select.projctn=TRUE, projctn='orthographic',
			title="", no.legend=FALSE,
			szRef=0.2,
			fileName=NULL, interactive.display=TRUE) {
#' function to map cases in an interactive map
#'
#' @param  data  data to be used
#' @param  select.projctn  argument to activate or deactivate the pulldown menu for selecting the type of projection
#' @param  projctn  initial type of map-projection to use, possible values are:
#' "equirectangular" | "mercator" | "orthographic" | "natural earth" | "kavrayskiy7" | "miller" | "robinson" | "eckert4" | "azimuthal equal area" | "azimuthal equidistant" | "conic equal area" | "conic conformal" | "conic equidistant" | "gnomonic" | "stereographic" | "mollweide" | "hammer" | "transverse mercator" | "albers usa" | "winkel tripel" | "aitoff" | "sinusoidal" 
#' @param  title  a string with a title to add to the plot
#' @param  szRef  numerical value to use as reference, to scale up the size of the bubbles in the map, from 0 to 1 (smmaller value --> larger bubbles)
#' @param  no.legend  parameter to turn off or on the legend on the right with the list of countries
#' @param  fileName  file where to save the HTML version of the interactive figure
#' @param  interactive.display  boolean argument for enabling or not displaying the figure
#'
#' @export
#'
#' @importFrom  plotly  plot_ly plot_geo %>% add_markers add_trace layout toRGB as_widget
#' @importFrom  htmlwidgets  saveWidget
#' @importFrom  utils  head str
#'
#' @examples
#' \dontrun{
#' # retrieve aggregated data
#' data <- covid19.data("aggregated")
#' # interactive map of aggregated cases -- with more spatial resolution
#' live.map(data)
# # or
# 'live.map()
#' 
#' # interactive map of the time series data of the confirmed cases
#' # with less spatial resolution, ie. aggregated by country
#' live.map(covid19.data("ts-confirmed"))
#' }
#'

	# load/check plotly
	loadLibrary("plotly")

	# identify columns
	country.col <- pmatch("Country",names(data))
	province.col <- pmatch("Province",names(data))
	long.col <- pmatch("Long",names(data))
	lat.col <- pmatch("Lat",names(data))

	col1 <- 5
	Ncols <- length(data)
	### depricated...
	if (tolower('status') %in% tolower(names(data))) {
		df <- data[data$status=="confirmed", c(province.col,country.col,lat.col,long.col,(Ncols-1))]
		for (sts in unique(data$status)) {
			if (tolower(sts) == "death") {
			# ie. deaths or recovered, hence we need to add them
				if (nrow(data[data$status==sts,]) == nrow(data[data$status=='confirmed',]) ) {
					df$total.deaths <- data[data$status==sts,(Ncols-1)]
				} else {
					df$total.deaths <- NA
				}
			} else if (tolower(sts) == "recovered") {
				if (nrow(data[data$status==sts,]) == nrow(data[data$status=='confirmed',]) ) { 
					df$total.recover <- data[data$status==sts,(Ncols-1)]
				} else {
					df$total.recover <- NA
				}
			} else {
				df$total.confirmed <- data[data$status == sts, (Ncols-1)]
			}
		
		}
		#Ncols <- Ncols-1
		#df <- data[data$status=="confirmed", c(1,2,3,4,(Ncols-1),(Ncols+1),(Ncols+2),(Ncols+3))]

		#print(head(df))
		# define description to display
		#text = "~paste(df$Province.State," - ",df$Country.Region,":", df$nbr.of.cases, " confirmed \n", )"
		hover.txt <- paste(df[,1]," - ",df[,2],'\n',
					"Confirmed:", df[,6],'\n',
					"Deaths: ",df[,7],'\n',
					#"Recovered: ",df[,8],'\n',
					#"Active cases: ",df[,6]-(df[,7]+df[,8]) )
					"Active cases: ",df[,6]-(df[,7]) )
		recorded.date <- names(data)[Ncols-1]
	} else {
		# check if time series data
		if (chk.TS.data(data)) {
			df <- data[,c(province.col,country.col,lat.col,long.col,Ncols,Ncols-1)]
			hover.txt <- paste(df[,1]," - ",df[,2],": ",df[,5],paste0("[",df[,5]-df[,6],"]") )
			#szRef <- 0.05
			recorded.date <- names(data)[Ncols]
		} else {
			df <- data[,c(province.col,country.col,lat.col,long.col)]
			df$total.confirmed <- data$"Confirmed"
			df$total.recovered <- data$"Recovered"
			df$total.deaths <- data$"Deaths"
			df$total.active <- data$"Active"

			hover.txt <- paste(df[,1]," - ",df[,2],'\n',
				"Confirmed: ", df[,5],'\n',
				"Recovered: ", df[,6],'\n',
				"Deaths: ", df[,7],'\n',
				"Active: ", df[,5]-(df[,6]+df[,7]),
						# df[,8],
						'\n')
			szRef <- .85
			recorded.date <- max(as.Date(data[,"Last_Update"]))
		}
	}

	# restauring column names
	names(df)[1] <- "Province.State"
	names(df)[2] <- "Country.Region"
	names(df)[3] <- "Lat"
	names(df)[4] <- "Long"
	names(df)[5] <- "nbr.of.cases"

	# protecting against some weirdeness in the data
	df[df[,5] < 0,5] <- NA

	#### geographical parameters
	g <- list(
	  scope = 'world',
	  projection = list(
	    type = projctn,
		#list(type = "mercator"),
		# 'natural earth',
		# 'orthographic',
	    rotation = list(lon = -100, lat = 40, roll = 0)
	  ),  
	  showland = TRUE,
	  showcountries=TRUE,
	  #showlakes = TRUE,
	  showocean = TRUE,
	  oceancolor = toRGB("steelblue"),
	  showlegend = TRUE,
	  landcolor = toRGB("gray15"),
	  showsubunits = TRUE,
	  #subunitwidth = 3,
	  #countrywidth = 2,
	  #subunitcolor = toRGB("green"),
	  #countrycolor = toRGB("white"),
	  bgcolor = toRGB("black")
	)
	####

	### figure creation
	fig <- plot_geo(df, locationmode = "country names", sizes = c(1, 250))

	# 1st LOG scale
	fig <- fig %>% add_markers(
		x = ~Long, y = ~Lat,
		size = ~log1p(nbr.of.cases),
		#color = ~nbr.of.cases,
		color = ~Country.Region,
		opacity = 0.65,
		colors = "Spectral", #"Set1", 
			#"RdGy", "RdBu",
			#"Dark2", "Set3", #"Paired",
			#"YlGnBu","Blues", #"Reds", #"Blues", #"Accent",
		# COLORS from RColorBrewer....
		#hoveron = "fills",
		marker=list(sizeref=szRef, sizemode="area",
				#sizes = c(~log1p(nbr.of.cases),~nbr.of.cases),
				line = list(width = 1, color = 'black')),
		hoverinfo="text",
		text = ~ hover.txt,
#		paste(df$Province.State," - ",df$Country.Region,'\n',
#				"Confirmed:", df[,6],'\n',
#				"Deaths: ",df[,7],'\n',
#				"Recovered: ",df[,8])
	)

	# linear scale
#	fig <- fig %>% add_markers(
#		x = ~Long, y = ~Lat,
#		size = ~((nbr.of.cases)),
#		color = ~Country.Region,
#		colors = "Spectral",
#		marker=list(sizeref=0.2, sizemode="area"),
#		hoverinfo="text",
#		text = ~ hover.txt,
#		visible = FALSE
#	)


	# linear scale
#        fig <- fig %>% add_markers(
#                x = ~Long, y = ~Lat,
#                size = ~((nbr.of.cases)),
#                color = ~Country.Region,
#                colors = "Spectral",
#                marker=list(sizeref=0.2, sizemode="area"),
#                hoverinfo="text",
#                text = ~ hover.txt,
#                visible = FALSE
#        )

	# add country names to the map
	cties <- unique(df$Country.Region)
	cties.lat <- c()
	cties.long <- c()
	for (i in cties) {
		cties.lat <- c(cties.lat, mean(df[df$Country.Region==i,"Lat"]) )
		cties.long <- c(cties.long, mean(df[df$Country.Region==i,"Long"]) )
	}

	## Set names of the cities on the map
	fig <- fig %>% add_trace(type="scattergeo", # view all scattergeo properties here: https://plot.ly/r/reference/#scattergeo
				lon = cties.long, lat = cties.lat, text = cties, mode="text",
				textposition = 'top right',
				textfont = list(color = toRGB("grey65"), size = 6),
				opacity=.75,
				showlegend = TRUE, name="Countries Names",
				visible = TRUE
				)
	###

	fig <- fig %>% layout(title = paste("covid19 ",title," - cases up to",recorded.date), geo=g,
				showlegend=!no.legend)


	###### MENUES ... aka "buttons" in plotly #######
	## dropdown
	projections.types <- c(	"equirectangular", "mercator", "orthographic", "natural earth","kavrayskiy7", "miller", "robinson", "eckert4", "azimuthal equal area","azimuthal equidistant", "conic equal area", "conic conformal", "conic equidistant", "gnomonic", "stereographic", "mollweide", "hammer", "transverse mercator", "albers usa", "winkel tripel" )
	projections = data.frame(type = projections.types)
	all_buttons <- list()
	for (i in 1:length(projections[,])) { 
		  all_buttons[[i]] <- list(method = "relayout",
						args = list(list(geo.projection.type = projections$type[i])),
						label = projections$type[i])
	}

	### update scale
	scale.buttons <- list(
			list(method = "update",
				args = list("marker", 'list(sizeref=0.2, sizemode="area")'),
				#args = list("size", "~log1p(df$nbr.of.cases)"),
				#args = list("visible", list(TRUE, FALSE, FALSE, TRUE)),
				label = "log"),
			list(method = "update",
				args = list("marker", 'list(sizeref=1, sizemode="area")'),
				#args = list("visible", list(FALSE, TRUE, FALSE, TRUE)),
				#args = list("size", "~nbr.of.cases"),
				label = "linear"),
			list(method="update",
				args = list("marker", 'list(sizeref=2, sizemode="area")'),
				#args = list("visible", list(FALSE, FALSE, TRUE, TRUE)),
				#args = list("size", 3),
				label = "constant")
			)

	## sliders for 'scrolling' longitude and latitude...
	lon_range = data.frame(seq(-180, 180, 10))
	lat_range = data.frame(seq(-90, 90, 10))
	colnames(lon_range) <- "x"
	colnames(lat_range) <- "x"

	all_lat <- list()
	for (i in 1:length(lat_range[,])) { 
		all_lat[[i]] <- list(method = "relayout",
			args = list(list(geo.projection.rotation.lat = lat_range$x[i])),
			label = lat_range$x[i])
	}

	all_lon <- list()
	for (i in 1:length(lon_range[,])) {  
		all_lon[[i]] <- list(method = "relayout", 
			args = list(list(geo.projection.rotation.lon = lon_range$x[i])),
			label = lon_range$x[i]) 
	} 

	# annotations
	annot <- list(x = 0, y=0.8, text = "Projection", yanchor = 'bottom', 
		xref = 'paper', xanchor = 'right',
		showarrow = FALSE)

	###################################################3


	# set background color
	fig <- fig %>% layout(plot_bgcolor='rgb(254, 247, 234)', paper_bgcolor='black')


	# set pull down menues and buttons

	# menu for projection selection
	if (select.projctn)
		fig <- fig %>% layout(
			updatemenus = list(
					list(active = 1, x = 0, y = 0.8, buttons=all_buttons)
					#list(active = -1, x = 0, y = 0.2, buttons=scale.buttons)
					)
		#	,
		#	sliders = list(
		#			list(
		#				active = (length(lon_range[,])-1)/2, 
		#				currentvalue = list(prefix = "Longitude: "), 
		#				pad = list(t = 20), 
		#				steps = all_lon
		#			),
		#
		#			list(
		#				active = (length(lat_range[,])-1)/2, 
		#				currentvalue = list(prefix = "Latitude: "), 
		#				pad = list(t = 100), 
		#				steps = all_lat
		#			)
		#		)
			)


#	fig <- fig %>% layout(
#  			plot_mapbox(), 
#			mapbox = list(style = "satellite") )


	# force displaying the figure
	if (interactive.display) print(fig)

	if (!is.null(fileName)) {
		FileName <- paste0(fileName,".html")
		# informing where the plot is going to be saved
		message("Saving interactive plot in ", FileName)
		htmlwidgets::saveWidget(as_widget(fig), FileName)
	}

	#if (TRUE) {
		return(fig)
	#} else {
		return(invisible(df))
	#}
}



##################################################################################





##################################################################################



time.series <- function(data) {


	for (i in 1:nrow(data)) {
		par(new=TRUE)
		plot(as.Date(names(data[5:45])),data[i,5:45], type='l', col=i)
	}


}



live.plot <- function(data) {


	fig <- plot_ly(data, x = ~colnames(data[,6:52]) )

	for (i in 1:nrow(data)) {
		fig <- fig %>% add_trace(y = ~data[i,6:52], name = as.character(data[i,2]),mode = 'markers')
	}

	fig

}


##################################################################################
