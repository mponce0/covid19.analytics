# Analysis fns of the covid19 package
#
# M.Ponce


#######################################################################


#######################################################################

tots.per.location <- function(data, geo.loc=NULL, confBnd=FALSE, nbr.plts=1, info="") {
#' function to compute totals per location
#'
#' @param  data  data.frame with *time series* data from covid19
#' @param  geo.loc  list of locations
#' @param  confBnd  flag to activate/deactivate drawing of confidence bands base on a moving average window
#' @param  nbr.plts  parameter to control the number of plots to display per figure
#' @param  info  additional info to display in plots' titles
#'
#' @return  a list or dataframe with totals per specified locations and type of case
#'
#' @export
#'
#' @importFrom  grDevices  rainbow
#' @importFrom  graphics   barplot par plot abline axis text
#'
#'
#' @examples
#' 
#' # read data for confirmed cases
#' data <- covid19.data("ts-confirmed")
#' # compute totals per location for all the countries
#' \donttest{
#' tots.per.location(data)
#' }
#' # compute totals per location for 'Italy'
#' tots.per.location(data,geo.loc="Italy")
#' # compute totals per location for 'Italy' and 'Germany'
#' tots.per.location(data,geo.loc=c("Italy","Germany"))
#'

	# check that the data is time series
	chk.TS.data(data,xtp=TRUE)

	# first column with daily data
	col1 <- 5

	# check the location indicated
	geo.loc <- checkGeoLoc(data,geo.loc)

	# dataframe to store results
	total.cases.per.country <- data.frame()

	### preserve user graphical env.
	# save the state of par() before running the code
	oldpar <- par(no.readonly = TRUE)
	# restore the previous state after the fn is done, even if it fails, so the user environment is not altered
	on.exit(par(oldpar))
	#########
	# set some graphical parameters
	set.plt.canvas(geo.loc,nbr.plts*2)
	###
	if (length(geo.loc) > 5) par(mar=c(1,1,1,1))
	#########


	# depricated
	# if the dataset contains all the posisble cases ('confirmed','recovered','deaths')
	# process them separatedly by using recursion...
	if ("status" %in% tolower(names(data))) {
		colN <- ncol(data)
		results.lst <- list()
		for (sts in unique(data$status) ) {
			message("Processing ",sts, " cases")
			results.lst <- list(results.lst, tots.per.location(data[data$status==sts,-colN],geo.loc,confBnd,nbr.plts,sts))
		}
		return(results.lst)
	}
	#######

	#######################
	# Aggregated (new) structure
	conditions <- c("confirmed","deaths","recovered","active")
	results.lst <- list()
	Country.col <- pmatch("Country", names(data))
	Province.col <- pmatch("Province", names(data))
	for (cond in conditions) {
		if ( (cond %in% tolower(names(data))) & ("FIPS" %in% names(data)) ) {
			col.X <- pmatch(cond, tolower(names(data)))
			message("Processing ",cond, " cases")
			results.lst <- list(results.lst, tots.per.location(data[,c(Province.col,Country.col,col.X)],geo.loc,confBnd,nbr.plts,cond))
		}
		if (length(results.lst) > 0)
			return(results.lst)
	}
	#######################
	#######################


	for (i in geo.loc) {
		cases.per.loc <- select.per.loc(data,i)
		colN <- ncol(cases.per.loc)
		if (tolower("status") %in% tolower(colnames(cases.per.loc))) {
			warning("Some internal inconsistency ocurred!",'\n',"Please check with the developer")
		} else {
			#print(cases.per.loc[,col1:colN])
			totals.per.loc.day <- apply(cases.per.loc[,col1:colN],MARGIN=2,sum)
			#print(totals.per.loc.day)
			if (toupper(i) != "ALL") {
				totals.per.loc <- sum(cases.per.loc[1:nrow(cases.per.loc),colN])
			} else {
				totals.per.loc <- apply(cases.per.loc[,col1:colN],MARGIN=2,sum)
			}
			cat(i,' -- ',totals.per.loc,'\n')
			total.cases.per.country <- rbind(total.cases.per.country,c(i,totals.per.loc.day,totals.per.loc))


			##### modelling
			yvar <- totals.per.loc.day
			#print(yvar)

			## LM models
			header("="," running models...")
			model.lm <- genModel(yvar,deg=1)
			model.exp <- genModel(log1p(yvar),deg=1)
			#print(str(model2))
			## GLM models
			model.poisson <- gen.glm.model(yvar,family="poisson")
			if (sum(yvar<=0)==0) model.gamma <- gen.glm.model(yvar)

			##### plots
			Ncols <- length(cases.per.loc)
			Nrows <- nrow(cases.per.loc)
			col0 <- 5
			x.dates <- as.Date(names(cases.per.loc)[col0:Ncols])
			y.cases <- cases.per.loc[1:Nrows , col0:Ncols]
			y.cases <- unlist(yvar)
			#names(y.cases) <- x.dates
			my.cols <- rep(rainbow(15L),each=20L)
			#print(x.dates)
			#print(y.cases)
			yvarlog1p <- log1p(unlist(y.cases))
			xvar <- 1:length(yvarlog1p)
			plot(log1p(unlist(y.cases)),
				#xlim=c(1,length(yvar)),
				#ylim=c(0,max(yvar,na.rm=TRUE)),
				type='b', xlab="days", ylab='nbr.of.cases (log)', pch=16L,col=my.cols)

			## adding models to plot
			#abline((model1), col='blue')
			abline((model.exp), col='red')
			abline(model.poisson, col='blue')
			if (sum(yvar<=0)==0) abline(model.gamma, col='green')
			# models legend
			#print(.9*max(yvarlog1p+0.1))
			text(.3*Ncols,0.8*max(yvarlog1p+0.1),
				paste("exp.model coefs: ",round(model.exp$coefficients[1],digits=3),";",round(model.exp$coefficients[2],digits=3)))
			text(.3*Ncols,0.725*max(yvarlog1p+0.1),
				paste("GLM-Poisson model coefs: ",round(model.poisson$coefficients[1],digits=3),";",round(model.poisson$coefficients[2],digits=3)))
			if (sum(yvar<=0)==0) text(0.3*Ncols,0.65*max(yvarlog1p+0.1),
				paste("GLM-Gamma model coefs: ",round(model.gamma$coefficients[1],digits=3),";",round(model.gamma$coefficients[2],digits=3)))

			### add confidence band based on moving Avg
			if (confBnd)
				confBand(xvar,yvarlog1p, 1,length(yvarlog1p),0,max(yvarlog1p,na.rm=TRUE), windowsNbr=10, lcolour='violet')

			#par(new=TRUE)
			barplot(unlist(y.cases), main=paste(i,info), col = my.cols)
			#confBand(xvar,yvar, 1,length(yvar),0,max(yvar,na.rm=TRUE), windowsNbr=10)
			# models legend
			text(.2*Ncols,0.85*max(unlist(y.cases)), paste("lm-exp GR = ",round(exp(model.exp$coefficients[2]),digits=2)))
			text(.2*Ncols,0.75*max(unlist(y.cases)), paste("glm-Poisson GR = ",round(exp(model.poisson$coefficients[2]),digits=2)))
			if (sum(yvar<=0)==0) text(.2*Ncols,0.65*max(yvar), paste("glm-Gamma GR = ",round(exp(model.gamma$coefficients[2]),digits=2)))
			#par(new=FALSE)
		}
	}

	# set the name of the columns
	names(total.cases.per.country) <- names(cases.per.loc[,col1:colN-1])
	# add the location
	total.cases.per.country <- cbind(geo.loc, total.cases.per.country)

	#plot(unlist(data[ data$Country.Region==i ,5:52]))
	return(invisible(total.cases.per.country))
}


#############################################################################


growth.rate <- function(data0, geo.loc=NULL, stride=1, info="") {
#' function to compute daily changes and "Growth Rates" per location; "Growth Rates" defined as the ratio between changes in consecutive days
#'
#' @param  data0  data.frame with *time series* data from covid19
#' @param  geo.loc  list of locations
#' @param  stride  how frequently to compute the growth rate in units of days
#' @param  info  additional information to include in plots' title
#'
#' @return  a list containing two dataframes: one reporting changes on daily baisis and a second one reporting growth rates, for the indicated regions
#'
#' @importFrom  gplots  heatmap.2 bluered
#' @importFrom  pheatmap  pheatmap
#'
#' @export
#'
#' @examples
#' ###\donttest{
#' # read data for confirmed cases
#' data <- covid19.data("ts-confirmed")
#' # compute changes and growth rates per location for all the countries
#' growth.rate(data)
#' # compute changes and growth rates per location for 'Italy'
#' growth.rate(data,geo.loc="Italy")
#' # compute changes and growth rates per location for 'Italy' and 'Germany'
#' growth.rate(data,geo.loc=c("Italy","Germany"))
#' ###}


	# check that the data is time series
	chk.TS.data(data0,xtp=TRUE)

	# define first column of data
	col1 <- 5

	cluster.type <- c("ALL","Country","City","Region","City/Region")

	# check on the location
	geo.loc <- checkGeoLoc(data,geo.loc)

	# where to store the results
	total.changes.per.day <-data.frame() 
	total.gr.per.day <- data.frame()

	### preserve user graphical env.
	# save the state of par() before running the code
	oldpar <- par(no.readonly = TRUE)
	# restore the previous state after the fn is done, even if it fails, so the user environment is not altered
	on.exit(par(oldpar))

	# set graphical output parameters
	set.plt.canvas(geo.loc,2)
	###
	if (length(geo.loc) > 5) par(mar=c(1,1,1,1))
	#########


	if ("status" %in% names(data0)) {
		results <- list()
		for (i in unique(data0$status) ) {
			message("Considering ",i, " cases")
			data <- data0[ data0$status == i, ]
			data <- data[, ! names(data) %in% "status", drop = F]
			result.per.case <- growth.rate(data,geo.loc,stride, i)
			results <- list(result.per.case, results)
		}
		return(results)
	} else {
		data <- data0
	}


	for (i in geo.loc) {
		# check whether the locations are countries/regions or provinces/states
		cases.per.loc <- select.per.loc(data,i)

                colN <- ncol(cases.per.loc)
                if (tolower("status") %in% tolower(cases.per.loc))
                         colN <- colN - 1

		cat("Processing... ",i,'\n')

		totals.per.loc <- apply(cases.per.loc[,col1:colN],MARGIN=2,sum)
		#print(totals.per.loc)

		# determine period of time and ranges...
		nbr.of.days <- length(totals.per.loc)
		range1 <- seq(1,nbr.of.days-1,stride)
		range2 <- range1+1

		# compute changes...
		changes <- totals.per.loc[range2]-totals.per.loc[range1]
		#print(changes)

		# compute growth rate
		gr.rate <- changes[range2]/changes[range1]
		# remove infinites in case of divison by 0 or something tiny...
		gr.rate[gr.rate == "Inf"] <- NA
		#print(gr.rate)

		# update resulting dataframe
		total.changes.per.day <- rbind(total.changes.per.day, changes)
		total.gr.per.day <- rbind(total.gr.per.day, gr.rate)

		# some graphic output...		
		my.cols <- rep(rainbow(15L),each=20L)
		x.dates <- as.Date(names(totals.per.loc[2:length(totals.per.loc)]))
		plot(x.dates,changes, type='b', xlab="time",ylab="Nbr of Changes", col=my.cols)
		#lines(x.dates,exp(model2$coefficients[2]*seq_along(x.dates))*model2$coefficients[1], col='blue')
		par(new=TRUE)
		plot(x.dates,log1p(changes), ylab='',xlab='', main=paste(i,info), type='b', pch=8, cex=.3, col=my.cols, lwd=2, lty=1, axes=FALSE)
		axis(4)
		par(new=FALSE)

		# check to see if there is enough data to display gr.rate
		if (sum(changes==0) < length(changes)-1) {
			plot(x.dates,unlist(gr.rate),
				axes=FALSE, xlab='',ylab='', ylim=c(0,max(gr.rate,na.rm=TRUE)*1.05),
				main=paste(i,info), type='b', col=my.cols)
			par(new=TRUE)
			barplot(unlist(gr.rate), ylab="Growth Rate",xlab="time",col = my.cols)
			axis(side=1,labels=FALSE)
			#axis.Date(side=1,x.dates)
			#box()
			par(new=FALSE)
		} else {
			message("Not enough data to compute growth rate for ",i)
		}
	}

	if ( (nrow(total.changes.per.day)>=1) && (length(geo.loc)>=1) )  {
		# load some graphical libraries 
		loadLibrary("pheatmap")
		loadLibrary("gplots")

		names(total.changes.per.day) <- names(changes)
		total.changes.per.day <- cbind(geo.loc, total.changes.per.day)

		names(total.gr.per.day) <- names(gr.rate)
		total.gr.per.day <- cbind(geo.loc, total.gr.per.day)

		par(mfrow=c(1,2))
		if (nrow(total.changes.per.day) > 1) {
			mat.tgt <- (as.matrix(total.changes.per.day[,2:length(changes)]))
			# Heatmap.2
			heatmap.2(mat.tgt,
				dendrogram="none", trace='none',
				col=bluered, labRow=geo.loc,
				main=paste("Changes per day",info) )

			# pheatmap
			pheatmap(mat.tgt,
				cluster_rows = FALSE, cluster_cols = FALSE,
				scale = 'none',
				annotation_legend = FALSE,
				drop_levels       = TRUE,
				fontsize          = 8,
				labels_col = "",
				labels_row = geo.loc,
				display_numbers = FALSE)

			# R basic heatmap
			#heatmap(mat.tgt,
			#	main=paste("Changes per day",info),
			#	labRow=geo.loc, Colv=NA,Rowv=NA,
			#	scale="column",
			#	col = topo.colors(256) )
		}

		if (nrow(total.gr.per.day) > 1) {
			mst.tgt <- (as.matrix(total.gr.per.day[,2:length(gr.rate)]))
			heatmap.2(mat.tgt,
				dendrogram="none", trace='none',
				col=bluered, labRow=geo.loc,
				main=paste("Growth Rate",info) )

			# pheatmap
			pheatmap(mat.tgt,
				cluster_rows = FALSE, cluster_cols = FALSE,
				display_numbers=FALSE,
				labels_col = "",
				labels_row = geo.loc)

			# R basic heatmap
			#heatmap(mat.tgt,
			#	main=paste("Growth Rate",info),
			#	labRow=geo.loc, Colv=NA,Rowv=NA,
			#	scale="column",
			#	col = rainbow(256) )
		}
	}

	return(list(Changes=total.changes.per.day,Growth.Rate=total.gr.per.day))
}

#############################################################################
#############################################################################
process.agg.cases <- function(data, Nentries, graphical.output) {

	comb.cols <- function(x,y) {

		return(c(x[1:2],y[1],x[3],y[2],x[4],y[3]))
	}


	if (graphical.output)
		par(mfrow=c(4,2))

	# define some cols and values...
	colN <- ncol(data)
	nRecords <- nrow(data)

	col.names <- names(data)
	country.col <- pmatch("Country", col.names)
	province.col <- pmatch("Province", col.names)
	date.col <- pmatch("Last_Update", col.names)

	col.conf <- pmatch("Confirmed",col.names)
	col.death <- pmatch("Deaths",col.names)
	col.recov <- pmatch("Recovered",col.names)
	col.activ <- pmatch("Active",col.names)
	col.cases <- c(col.conf,col.death,col.recov,col.activ)

	# if Nentries i set to 0, will consider *all* entries                
	if (Nentries==0) Nentries <- nRecords

	cases <- c("Confirmed","Deaths","Recovered","Active")
	for (i in col.cases) {
		header("#")
		report.title <- paste("AGGREGATED Data  -- ORDERED BY ",toupper(col.names[i]),"Cases  -- Data dated: ",
					as.character(max(as.Date(data[,date.col]))) ,
					" :: ",as.character(Sys.time()) )
		header('',paste0("##### ",report.title))
		header("#")

		header('',paste0("Total number of Countries/Regions affected: ",length(unique(data[,country.col]))))
		header('',paste0("Total number of Cities/Provinces affected: ",length(unique(data[,province.col]))))
		header("-")

		# get percentage cases
		cols.perc <- c()
		for (j in col.cases[1:4]) {
			if (j != col.conf) {
				data[,paste0("Perc.",col.names[j])]  <- round((data[,j]/data[,col.conf])*100,2)
			} else {
				 data[,paste0("Perc.",col.names[j])]  <- round((data[,j]/sum(data[,col.conf]))*100,2)
			}
			cols.perc <- c(cols.perc,ncol(data))
		}

		# top countries/regions
		#data.ordered <- data[order(data[,i],decreasing=TRUE),][1:Nentries,c(country.col,province.col, col.cases, cols.perc)]
		custom.list <- comb.cols(col.cases,cols.perc)
		data.ordered <- data[order(data[,i],decreasing=TRUE),][1:Nentries,c(country.col,province.col, custom.list)]
#                names(data.ordered) <- c("Country.Region","Province.State", cases)

                print(data.ordered)

		header("=")

		target.col <- col.names[i]
		ord.cty.col <- pmatch("Country",names(data.ordered))
		ord.prv.col <- pmatch("Province",names(data.ordered))
                # graphics
                if (graphical.output) {
                        legends <- paste(as.character(data.ordered[,ord.cty.col]),as.character(data.ordered[,ord.prv.col]),'\n',data.ordered[,target.col])
                        color.scheme <- heat.colors(Nentries)   #topo.colors(Nentries)
                                        #terrain.colors(Nentries)       #rainbow(Nentries)

                        pie(data.ordered[,target.col],
                                labels=legends,
                                main=substr(report.title,1,floor(nchar(report.title)/2)),
                                col=color.scheme )

                        #par(new=TRUE)
                        #par(mfrow=c(1,2))
                        barplot(data.ordered[,target.col], names.arg=legends,
                                col=color.scheme,
                                main=substr(report.title,ceiling(nchar(report.title)/2),nchar(report.title)) )
                }

		# remove perc cols
		data <- data[,-cols.perc]
	}
}


#########################################################################


report.summary <- function(cases.to.process="ALL", Nentries=10, graphical.output=TRUE, saveReport=FALSE) {
#' function to summarize the current situation, will download the latest data and summarize the top provinces/cities per case 
#'
#' @param  cases.to.process  which data to process: "TS" --time series--, "AGG" --aggregated-- or "ALL" --time series and aggregated--
#' @param  Nentries  number of top cases to display
#' @param  graphical.output  flag to deactivate graphical output
#' @param  saveReport  flag to indicate whether the report should be saved in a file
#'
#' @importFrom  grDevices  heat.colors
#' @importFrom  graphics  pie
#'
#' @export
#'
#' @examples
#' # displaying top 10s
#' report.summary()
#'
#' # get the top 20
#' report.summary(20)
#'

	Ndefault <- 10

	if (!is.numeric(Nentries)) {
		stop("The argument to this function must be a number!")
	} else if (Nentries < 0) {
		message("Argument must be positive or zero (for listing all entries)!",'\n',
			"Will assume default value:",Ndefault)
		Nentries <- Ndefault
	}


	# preserve user graphical environment
	if (graphical.output) {
		old.par <- par(no.readonly=TRUE)
		on.exit(par(old.par))
	}

	# save report to a file
	if (saveReport) {
			fileName <- paste0("covid19-SummaryReport_",Sys.Date(),".txt")
			sink(file=fileName,split=TRUE)
	}

	##### PROCESS TIME SERIES DATA ######
	if ( (toupper(cases.to.process)=="ALL") | (toupper(cases.to.process)=="TS") ) {
	# first column with cases data
	col1 <- 5

	#cases <- c("confirmed","recovered","deaths")
	cases <- c("ts-confirmed","ts-recovered","ts-deaths")	#, "aggregated")
	for (i in cases) {
		# read data
		data <- covid19.data(i)

		# if Nentries i set to 0, will consider *all* entries
		if (Nentries==0) Nentries <- nrow(data)

		colN <- ncol(data)
		nRecords <- nrow(data)
#		if (grepl("aggregated",i))	col.Tgt <-

		header("#")
		report.title <- paste(toupper(i),"Cases  -- Data dated: ",names(data)[colN]," :: ",as.character(Sys.time()))
		header('',paste0("##### ",report.title))
		header("#")

		country.col <- pmatch("Country", names(data))
		province.col <- pmatch("Province", names(data))
		header('',paste("Total number of Countries/Regions affected: ",length(unique(data[,country.col]))) )

		header('',paste("Total number of Cities/Provinces affected: ",length(unique(data[,province.col]))) )

		header("-")

		# Totals per countries/cities
		#data$Totals <- apply(data[,col1:colN],MARGIN=1,sum)
		data$Totals <- data[,colN]

		# store total nbr of cases
		if (grepl("confirmed",i)) {
			geo.list <- data[,c(country.col,province.col)]
                        total.cases <- data[,ncol(data)]
			colsF <- (colN+1):(colN+2)
			total.global <- sum(total.cases)
			data$GlobalPerc <- round((total.cases/total.global)*100,2)
		} else {
			# check that the countries/regions match in order to compute percentages...
			# Could improve partial matches...
			if ( sum(as.character(data[,country.col])==as.character(geo.list[1:nRecords,1]))==nRecords
				&&  sum(as.character(data[,province.col])==as.character(geo.list[1:nRecords,2]))==nRecords ) {
				# get percentage cases
				data$Perc  <- round((data[,colN]/total.cases[1:nRecords])*100,2)
				colsF <- (colN+1):(colN+2)
			} else {
				colsF <- colN+1
			}
		}
		# top countries/regions
		data.ordered <- data[order(data$Totals,decreasing=TRUE),][1:Nentries,c(country.col,province.col,colsF)]
		names(data.ordered)[1:3] <- c("Country.Region","Province.State","Totals")
	
		print(data.ordered)

		# Average percentages...
		Perc.col <- which(grepl("Perc",names(data)))
		if (length(Perc.col)>0) {
			header('-')
			#print(data[,Perc.col])
			header("",paste("Global Perc. Average: ",(mean(data[,Perc.col],na.rm=TRUE))) )
		}
		Perc.col.top <- which(grepl("Perc",names(data.ordered)))
		if (length(Perc.col.top)>0) {
			header("",paste("Global Perc. Average in top ",Nentries,": ",(mean(data.ordered[,Perc.col.top],na.rm=TRUE))) )		
		}

		header("=")

		# graphics
		if (graphical.output) {
			legends <- paste(data.ordered$Country.Region,data.ordered$Province.State,'\n',data.ordered$Totals)
			color.scheme <- heat.colors(Nentries)	#topo.colors(Nentries)
					#terrain.colors(Nentries)	#rainbow(Nentries)

			par(mfrow=c(1,2))
			#par(mfrow=c(1,1))

			pie(data.ordered$Totals,
				labels=legends,
				main=substr(report.title,1,floor(nchar(report.title)/2)),
				col=color.scheme )

			#par(new=TRUE)
			#par(mfrow=c(1,2))
			barplot(data.ordered$Totals, names.arg=legends,
				col=color.scheme,
				main=substr(report.title,ceiling(nchar(report.title)/2),nchar(report.title)) )
		}
	}
	}

	##### PROCESS "AGREGATED" DATA  #######
	if ( (toupper(cases.to.process)=="ALL") | (toupper(cases.to.process)=="AGG") ) {
		process.agg.cases(covid19.data("aggregated"), Nentries, graphical.output)
	}

	if (saveReport) {
		sink()
		message("Report saved in ",fileName)
	}
}

#############################################################################
