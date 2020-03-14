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
		total.cases <- covid19.data()
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

live.map <- function(data=covid19.data(), projctn='orthographic', title="") {
#' function to map cases in an interactive map
#'
#' @param  data  data to be used
#' @param  projctn  initial type of map-projection to use, possible values are:
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
					"Recovered: ",df[,8],'\n',
					"Active cases: ",df[,6]-(df[,7]+df[,8]) )
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
	scalingFactor <- 4000
	scaled.nbr.of.cases <- df$nbr.of.cases*scalingFactor

	fig <- plot_geo(df, locationmode = "country names", sizes = c(1, 250))

	fig <- fig %>% add_markers(
		x = ~Long, y = ~Lat,
		size = ~(log1p(nbr.of.cases)),
		#color = ~nbr.of.cases,
		color = ~Country.Region,
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

	# add country names
	cties <- unique(df$Country.Region)
	cties.lat <- c()
	cties.long <- c()
	for (i in cties) {
		cties.lat <- c(cties.lat, mean(df[df$Country.Region==i,"Lat"]) )
		cties.long <- c(cties.long, mean(df[df$Country.Region==i,"Long"]) )
	}

	fig <- fig %>% add_trace(type="scattergeo", # view all scattergeo properties here: https://plot.ly/r/reference/#scattergeo
				lon = cties.long, lat = cties.lat, text = cties, mode="text",
				textposition = 'top right',
				textfont = list(color = toRGB("grey65"), size = 6),
				opacity=.75,
				showlegend = TRUE, name="Countries Names"
)


	fig <- fig %>% layout(title = paste("covid19 ",title," - cases up to",names(data)[Ncols-1]), geo=g)


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
				args = list("size", "~log1p(nbr.of.cases)"),
				label = "log"),
			list(method = "update",
				args = list("size", "~(nbr.of.cases)"),
				label = "linear"),
			list(method="update",
				args = list("size", ""),
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

	fig <- fig %>% layout(
			updatemenus = list(
					list(active = 1, x = 0, y = 0.8, buttons=all_buttons),
					list(active = 2, x = 0, y = 0.2, buttons=scale.buttons)
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
