# "covid19" graphical functions
#
# M.Ponce

##################################################################

totals.plt <- function(data0=NULL, geo.loc=NULL, interactive.fig=TRUE) {
#' function to plot total number of cases per day for different groups
#'
#' @param  data  dataset to process, default all the possible cases: 'confirmed', 'recovered' and 'deaths' for all countries/regions
#' @param  geo.loc  geographical location, country/region or province/state to restrict the analysis to
#' @param  interactive.fig  swith to turn off/on an interactive plot
#'
#' @export
#'

	if (is.null(data0)) {
		total.cases <- covid19()
	} else {
		total.cases <- data0
	}

	if (!is.null(geo.loc)) {
		# check geographical location
		geo.loc <- checkGeoLoc(total.cases,geo.loc)
		total.cases <- select.per.loc(total.cases,geo.loc)
	}

	col1 <-5; colN <- ncol(total.cases)
	# check whether is the whole dataset...
	if (tolower("status") %in% colnames(total.cases)) {
		all.cases <- TRUE
		colN <- colN-1

		###
		categories <- unique(total.cases$status)
		totals.per.cat <- data.frame()
		for (categ in seq_along(categories)) {
			totals.per.cat <- c(totals.per.cat, c(categories[categ],apply(total.cases[total.cases$status==categ,col1:colN], MARGIN=2,sum) )  )
		}
		##
		confirmed <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
		recovered <- apply(total.cases[total.cases$status=="recovered",col1:colN], MARGIN=2,sum)
		deaths <- apply(total.cases[total.cases$status=="death",col1:colN], MARGIN=2,sum)
		###
	} else {
		all.cases <- FALSE
	}

	totals <- apply(total.cases[,col1:colN], MARGIN=2,sum)

	x.dates <- as.Date(names(total.cases)[col1:colN])
	ymax <- max(totals,na.rm=TRUE)

	### STATIC PLOTS
	plot(x.dates, totals, ylim=c(0,ymax),  type='l', col='darkred',
		xlab='time', ylab='nbr of cases', main=geo.loc)

	if (all.cases) {
		par(new=TRUE,ann=FALSE)
		plot(x.dates,confirmed, ylim=c(0,ymax), axes=FALSE, type='b', col='black')
		par(new=TRUE,ann=FALSE)
		plot(x.dates,recovered, ylim=c(0,ymax), axes=FALSE, type='b', col='blue')
		par(new=TRUE,ann=FALSE)
		plot(x.dates,deaths, ylim=c(0,ymax), axes=FALSE, type='b', col='red')
	}

	### INTERACTIVE PLOTS
	if (interactive.fig) {
		# load/check plotly
		loadLibrary("plotly")

		# define interactive figure/plot
		totals.ifig <- plot_ly(total.cases, x = ~colnames(total.cases[,col1:colN]))	#, type='scatter', mode='line+markers')
		if (all.cases) {
#			for (categ in categories) {
#				fig <- fig %>% add_trace(y = ~categ, name="confirmed", mode='line+markers')
#			}
			totals.ifig <- totals.ifig %>% add_trace(y = ~confirmed, name="confirmed", type='scatter', mode='lines+markers')
			totals.ifig <- totals.ifig %>% add_trace(y = ~recovered, name="recovered", type='scatter', mode='lines+markers')
			totals.ifig <- totals.ifig %>% add_trace(y = ~deaths, name="deaths", type='scatter', mode='lines+markers')
		} else {
			totals.ifig <- totals.ifig %>% add_trace(y = ~totals, name=geo.loc, type='scatter', mode='lines+markers')
		}
	
		# activate interactive figure
		print(totals.ifig)
	}

	#return(totals.per.cat)
}


##################################################################################

live.map <- function(data=covid19(), projctn='orthographic', title="") {
#' function to map cases in an interactive map
#'
#' @param  data  data to be used
#' @param  projctn  type of map-projection to use, possible values are:
#' "equirectangular" | "mercator" | "orthographic" | "natural earth" | "kavrayskiy7" | "miller" | "robinson" | "eckert4" | "azimuthal equal area" | "azimuthal equidistant" | "conic equal area" | "conic conformal" | "conic equidistant" | "gnomonic" | "stereographic" | "mollweide" | "hammer" | "transverse mercator" | "albers usa" | "winkel tripel" | "aitoff" | "sinusoidal" 
#' @param  title  a string with a title to add to the plot
#'
#' @export
#'

	# load/check plotly
	loadLibrary("plotly")

	col1 <- 5
	Ncols <- length(data)
	if (tolower('status') %in% tolower(names(data))) {
		df <- data[data$status=="confirmed", c(1,2,3,4,(Ncols-1))]
		for (sts in unique(data$status)) {
			if (tolower(sts) == "death") {
			# ie. deaths or recovered, hence we need to add them
				df$total.deaths <- data[data$status==sts,(Ncols-1)]
				df$total.deaths <- data[data$status==sts,(Ncols-1)]
			} else if (tolower(sts) == "recovered") {
				df$total.recover <- data[data$status==sts,(Ncols-1)]
			} else {
				df$total.confirmed <- data[data$status == sts, (Ncols-1)]
			}
		
		}
		#Ncols <- Ncols-1
		#df <- data[data$status=="confirmed", c(1,2,3,4,(Ncols-1),(Ncols+1),(Ncols+2),(Ncols+3))]

		print(head(df))
		# define description to display
		#text = "~paste(df$Province.State," - ",df$Country.Region,":", df$nbr.of.cases, " confirmed \n", )"
		hover.txt <- paste(df$Province.State," - ",df$Country.Region,'\n',
					"Confirmed:", df[,6],'\n',
					"Deaths: ",df[,7],'\n',
					"Recovered: ",df[,8])
	} else {
		df <- data[,c(1,2,3,4,Ncols)]
		hover.txt <- paste(df$Province.State," - ",df$Country.Region,": ",df[,5]) 
	}

	names(df)[5] <- "nbr.of.cases"

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

	fig <- fig %>% add_markers(
		x = ~Long, y = ~Lat, size=~(nbr.of.cases*50), color=~Country.Region,
		colors = "Spectral", #"Set1", 
			#"RdGy", "RdBu",
			#"Dark2", "Set3", #"Paired",
			#"YlGnBu","Blues", #"Reds", #"Blues", #"Accent",
		# COLORS from RColorBrewer....
		#hoveron = "fills",
		hoverinfo="text",
		text = ~ hover.txt,
#		paste(df$Province.State," - ",df$Country.Region,'\n',
#				"Confirmed:", df[,6],'\n',
#				"Deaths: ",df[,7],'\n',
#				"Recovered: ",df[,8])
	)

	fig <- fig %>% layout(title = paste("covid19 ",title," - cases up to",names(data)[Ncols-1]), geo=g)

	# set background color
	fig <- fig %>% layout(plot_bgcolor='rgb(254, 247, 234)', paper_bgcolor='black')

#	fig <- fig %>% layout(
#  			plot_mapbox(), 
#			mapbox = list(style = "satellite") )

	print(fig)

	return(df)
}



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
