# Analysis fns of the covid19.analytics package
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


growth.rate <- function(data0, geo.loc=NULL, stride=1, info="", staticPlt=TRUE, interactiveFig=FALSE, interactive.display=TRUE) {
#' function to compute daily changes and "Growth Rates" per location; "Growth Rates" defined as the ratio between changes in consecutive days
#'
#' @param  data0  data.frame with *time series* data from covid19
#' @param  geo.loc  list of locations
#' @param  stride  how frequently to compute the growth rate in units of days
#' @param  info  additional information to include in plots' title
#' @param  staticPlt  boolean flag to indicate whether static plots would be generated or not
#' @param  interactiveFig  boolean flag to indicate whether interactice figures would be generated or not
#' @param  interactive.display  boolean flag to indicate whether the interactive plot will be displayed (pushed) to your browser
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
#' # growth.rate(data)
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
	geo.loc <- checkGeoLoc(data0,geo.loc)

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
			result.per.case <- growth.rate(data,geo.loc,stride, i, staticPlt=staticPlt, interactiveFig=interactiveFig, interactive.display=interactive.display)
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
		gr.rate[gr.rate == "-Inf"] <- NA
		#print(gr.rate)

		# update resulting dataframe
		total.changes.per.day <- rbind(total.changes.per.day, changes)
		total.gr.per.day <- rbind(total.gr.per.day, gr.rate)

		if (staticPlt) {
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
			# mask for avoiding blowing-up the plots and set the yMaximum value
			mask <- (!is.infinite(gr.rate) & !is.nan(gr.rate) & !is.na(gr.rate))
			yMax <- max(gr.rate[mask],na.rm=TRUE)
			plot(x.dates,unlist(gr.rate),
				axes=FALSE, xlab='',ylab='', ylim=c(0,yMax*1.05),	#max(gr.rate,na.rm=TRUE)*1.05),
				main=paste(i,info), type='b', col=my.cols)
			par(new=TRUE)
			barplot(unlist(gr.rate), ylab="Growth Rate",xlab="time",col = my.cols, ylim=c(0,yMax*1.05))
			axis(side=1,labels=FALSE)
			#axis.Date(side=1,x.dates)
			#box()
			par(new=FALSE)
		} else {
			message("Not enough data to compute growth rate for ",i)
		}
		}
	}

	if ( (nrow(total.changes.per.day)>=1) && (length(geo.loc)>=1) )  {

		names(total.changes.per.day) <- names(changes)
		total.changes.per.day <- cbind(geo.loc, total.changes.per.day)

		names(total.gr.per.day) <- names(gr.rate)
		total.gr.per.day <- cbind(geo.loc, total.gr.per.day)

	if (staticPlt) {
		# load some graphical libraries 
                loadLibrary("pheatmap")
                loadLibrary("gplots")

		par(mfrow=c(1,2))
		if (nrow(total.changes.per.day) > 1) {
			mat.tgt <- (as.matrix(total.changes.per.day[,2:length(changes)]))
			# deal with NA/nan/... setting to 0
			mat.tgt[is.na(mat.tgt)] <- 0

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
			mat.tgt <- (as.matrix(total.gr.per.day[,2:length(gr.rate)]))
			#mat.tgt <-  mat.tgt[,which(unlist(lapply(mat.tgt, function(x) !all(is.na(x)))))]
			#mat.tgt <-  mat.tgt[,which(unlist(lapply(mat.tgt, function(x) !all(is.nan(x)))))]
			# deal with NA/nan/... setting to 0
			mat.tgt[is.na(mat.tgt)] <- 0
			mat.tgt[is.nan(mat.tgt)] <- 0

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
}
	if (interactiveFig) {
		loadLibrary("plotly")
		p1 <- plot_ly(z=t(total.changes.per.day),x=total.changes.per.day['geo.loc'][[1]],y=names(total.changes.per.day), type='surface', scene='scene1')
		p2 <- plot_ly(z=t(total.changes.per.day),x=total.changes.per.day['geo.loc'][[1]],y=names(total.changes.per.day), type='heatmap')
		p3 <- plot_ly(z=t(total.gr.per.day), x=total.gr.per.day['geo.loc'][[1]], y=names(total.changes.per.day)[-1], type='heatmap')
		#p3 <- layout(p3, scene = list(zaxis = list(title = "Growth Rate", range = c(-4,4)), yaxis = list(title = "B"), xaxis = list(title = "C")))
		if (interactive.display) {
					print(p1)
					print(p2)
					print(p3)
		} else {
			# compose one plot

			ifig <- subplot(p2, p1, p3)#, widths=c(0.3,0.4,0,.3), ncols=3)
			ifig <- ifig %>% layout(title = "Daily Changes & Growth Rate",
         			scene1 = list(domain=list(x=c(0,0.5),y=c(0.75,1)),
                      				aspectmode='cube')
				#scene2 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),
 		                #     aspectmode='data'),
				#scene3 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),
				#	aspectmode='data')
					)

			#ifig <- subplot(list(subplot(p1,p2),p3), nrows = 3)#, shareX = FALSE, titleX = TRUE)
			return(ifig)
		}
	}
	return(list(Changes=total.changes.per.day,Growth.Rate=total.gr.per.day))
}


#############################################################################
#############################################################################
#### AUXILIARY FUNCTIONS FOR REPORTS

        ######################

        intro.header <- function(i, data,colN,country.col,province.col, geo.loc=NULL, scr.len=80) {

                header("#", total.len=scr.len)
                report.title <- paste(toupper(i),"Cases  -- Data dated: ",names(data)[colN]," :: ",as.character(Sys.time()))
                header('',paste0("##### ",report.title))
                header("#", total.len=scr.len)

                country.col <- pmatch("Country", names(data))
                province.col <- pmatch("Province", names(data))
                header('',paste("Number of Countries/Regions reported: ",length(unique(data[,country.col]))) )
                header('',paste("Number of Cities/Provinces reported: ",length(unique(data[,province.col]))) )
                header('',paste0("Unique number of distinct geographical locations combined: ",nrow(unique(data[,c(country.col,province.col)]))))
                header("-", total.len=scr.len)

		if (length(colN)==1) {
			if (is.null(geo.loc)) {
				locn <-"Worldwide" 
			} else {
				locn <- "For selected locations"
			}
			header('',paste(locn,i," Totals:", sum(data[,colN],na.rm=TRUE)))
			#header('',paste(Nentries," Top Totals:", sum(data[,colN],na.rm=TRUE)))
			header('-', total.len=scr.len)
		} else {
			for (j in colN) {
				header('',paste("Worldwide ",i," Totals:", sum(data[,j],na.rm=TRUE)))
			}
			#header('-', total.len=scr.len)
		}

                return(invisible(report.title))
        }

        ######################

        report.Avgs <- function(data,target="Perc",descr="") {

                Perc.col <- which(grepl(target,names(data)))
                if (length(Perc.col)>0) {
                        #print(data[,Perc.col])
			tgt.col <- data[,Perc.col]
			tgt.col <- tgt.col[!is.infinite(tgt.col)]
			#print(tgt.col)
                        mean.value <- round(mean(tgt.col,na.rm=TRUE),2)
                        sd.value <- round(sd(tgt.col,na.rm=TRUE),2)
                        if (!is.infinite(mean.value) & !is.na(mean.value))
                                header( "", paste(descr,mean.value, paste0("(sd: ",sd.value,")")) )
                }

        }

        #####################

	report.Totals <- function(cases,totals,preTitle="",totEntries=NA) {
		header('',eol='\n\n')
		header('*')
		header('*',"OVERALL SUMMARY")
		header('*')
		#print.Title <- TRUE
		for (j in names(totals)) {
			values <- totals[[j]]
			#if (print.Title) {
				header("", paste("**** ",preTitle,toupper(j),"****"))
			#	print.Title <- TRUE
			#}
			header('',paste("\t",paste(cases,collapse="\t")))
			header('',paste("\t",paste(round(values,2),collapse="\t")))
			header('',paste0("\t\t\t",paste(round(values[2:3]/(values[1])*100,2),collapse="% \t\t"),"%"))
                        ##header('',paste0("\t",paste(round(values/(totals[["tots"]][1])*100,2),collapse="% \t"),"%"))
			#header('-')
		}

		if (length(totEntries)>1 || !is.na(totEntries))
			header('',paste('\n\n',"* Statistical estimators computed considering",
					paste(totEntries,collapse="/"),
					"independent reported entries per case-type"))

		header('*')
	}

#############################################################################
#############################################################################
process.agg.cases <- function(data, Nentries, geo.loc=NULL, graphical.output) {

	#######

	comb.cols <- function(x,y) {

		#return(c(x[1:2],y[1],x[3],y[2],x[4],y[3]))
		return(c(x[1],y[1], x[2],y[2], x[3],y[3], x[4],y[4]))
	}

	#######

	# set the length in chars for the output
	scr.len <- 140

	if (graphical.output)
		par(mfrow=c(4,2))


	# clean some spurious data
	#data <- na.omit(data[,-1])

	# define some cols and values...
	colN <- ncol(data)
	nRecords <- nrow(data)

	col.names <- names(data)
	country.col <- pmatch("Country", col.names)
	province.col <- pmatch("Province", col.names)
	date.col <- pmatch("Last_Update", col.names)
	combKey.col <- pmatch("Combined",col.names)

	col.conf <- pmatch("Confirmed",col.names)
	col.death <- pmatch("Deaths",col.names)
	col.recov <- pmatch("Recovered",col.names)
	col.activ <- pmatch("Active",col.names)
	col.cases <- c(col.conf,col.death,col.recov,col.activ)

	# if Nentries i set to 0, will consider *all* entries                
	if (Nentries==0) Nentries <- nRecords

	data0 <- data

	# allowing to report by selected location
	if (!is.null(geo.loc)) {
		# check the location indicated
		geo.loc <- checkGeoLoc(data,geo.loc)
		data <- select.per.loc(data,geo.loc)
		Nentries <- min(Nentries,nrow(data))
	}
	###

	cases <- c("Confirmed","Deaths","Recovered","Active")
	for (i in col.cases) {

		header("#", total.len=scr.len)
		report.title <- paste("AGGREGATED Data  -- ORDERED BY ",toupper(col.names[i]),"Cases  -- Data dated: ",
					as.character(max(as.Date(data[,date.col]))) ,
					" :: ",as.character(Sys.time()) )
		header('',paste0("##### ",report.title))
		header("#", total.len=scr.len)

		header('',paste0("Number of Countries/Regions reported: ",length(unique(data[,country.col]))))
		header('',paste0("Number of Cities/Provinces reported: ",length(unique(data[,province.col]))))
		header('',paste0("Unique number of distinct geographical locations combined: ",
				length(unique(unlist(data[,combKey.col])))
				#nrow(unique(data[,c(country.col,province.col)]))
				) )
		header("-", total.len=scr.len)

		# display 'header'
		#report.title <- intro.header(i, data,col.cases[i],
		#				country.col,province.col, scr.len=scr.len)


		# get percentage cases
		cols.perc <- c()
		for (j in col.cases[1:4]) {
			if (j != col.conf) {
				data[,paste0("Perc.",col.names[j])]  <- round((data[,j]/data[,col.conf])*100,2)
			} else {
				# Global Perc.
				data[,paste0("Perc.",col.names[j])]  <- round((data[,j]/sum(data0[,col.conf]))*100,2)
				# Relative Perc.
				#data[,paste0("Rel.Perc.",col.names[j])]  <- round((data[,j]/sum(data[,col.conf]))*100,2)
			}
			cols.perc <- c(cols.perc,ncol(data))
		}

		# top countries/regions
		#data.ordered <- data[order(data[,i],decreasing=TRUE),][1:Nentries,c(country.col,province.col, col.cases, cols.perc)]
		custom.list <- comb.cols(col.cases,cols.perc)
		#print(custom.list)

		# Select which columns to keep from the original dataframe
		original.cols <- c(combKey.col)
		upto.record <- min(nrow(data),Nentries)
		#original.cols <- c(country.col,province.col,combKey.col)
		data.ordered <- data[order(data[,i],decreasing=TRUE),][1:upto.record,c(original.cols, custom.list)]
		#names(data.ordered) <- c("Country.Region","Province.State", cases)
		names(data.ordered)[1] <- "Location"
		row.names(data.ordered) <- 1:upto.record

                print(data.ordered)

		header("=", total.len=scr.len)

		target.col <- col.names[i]
		ord.cty.col <- pmatch("Country",names(data.ordered))
		ord.prv.col <- pmatch("Province",names(data.ordered))
		##ord.combKey.col <- pmatch("Combined",names(data.ordered))
		ord.combKey.col <- which("Location"==names(data.ordered))

#print("#!!!")
#filter.data <- na.omit(data.ordered[data.ordered[,target.col]>0,target.col])
#print(filter.data)
#print("#!!!")

                # graphics
                if (graphical.output) {
			#legends <- paste(as.character(data.ordered[,ord.cty.col]),as.character(data.ordered[,ord.prv.col]),'\n',data.ordered[,target.col])
			legends <- paste(as.character(data.ordered[,ord.combKey.col]),'\n',data.ordered[,target.col])
                        color.scheme <- heat.colors(Nentries)   #topo.colors(Nentries)
                                        #terrain.colors(Nentries)       #rainbow(Nentries)

			filter.data <- na.omit(data.ordered[data.ordered[,target.col]>0,target.col])

                        #pie(na.omit(data.ordered[,target.col]),
			pie(filter.data,
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

	## REPORT TOTALS
	if (TRUE) {
		#print(names(data)[col.cases])
		header('',paste("\t",paste(names(data)[col.cases],collapse='\t')))
		header('',"Totals",eol='')
		header('',paste("\t",paste(sapply(data[,col.cases],sum),collapse='\t')))
		header('',"Average",eol='')
		header('',paste("\t",paste(round(sapply(data[,col.cases],mean),2),collapse='\t')))
		header('',"Standard Deviation",eol='')
		header('',paste("\t",paste(round(sapply(data[,col.cases],sd),2),collapse="\t")))

		header('',paste('\n\n',"* Statistical estimators computed considering",nrow(data),"independent reported entries"))
	}
}


#########################################################################


report.summary <- function(cases.to.process="ALL", Nentries=10, geo.loc=NULL,
				graphical.output=TRUE, saveReport=FALSE) {
#' function to summarize the current situation, will download the latest data and summarize the top provinces/cities per case 
#'
#' @param  cases.to.process  which data to process: "TS" --time series--, "AGG" --aggregated-- or "ALL" --time series and aggregated--
#' @param  Nentries  number of top cases to display
#' @param  geo.loc  geographical location to process
#' @param  graphical.output  flag to deactivate graphical output
#' @param  saveReport  flag to indicate whether the report should be saved in a file
#'
#' @importFrom  stats  sd
#' @importFrom  grDevices  heat.colors
#' @importFrom  graphics  pie
#'
#' @export
#'
#' @examples
#' # triggers CRAN checks for timing
#' \dontrun{
#' # displaying top 10s
#' report.summary()
#'
#' # get the top 20
#' report.summary(Nentries=20,graphical.output=FALSE)
#'
#' # specify a location
#' report.summary(geo.loc="NorthAmerica")
#' }
#'

	# reassign Nentries to Nentriex
	Nentriex <- Nentries
	Ndefault <- 10

	if (!is.numeric(Nentriex)) {
		stop("The argument to this function must be a number!")
	} else if (Nentriex < 0) {
		message("Argument must be positive or zero (for listing all entries)!",'\n',
			"Will assume default value:",Ndefault)
		Nentriex <- Ndefault
	}


	# preserve user options
	old.opts <- options("width")
	options(width=200)

	# preserve user graphical environment
	if (graphical.output) {
		old.par <- par(no.readonly=TRUE)
		on.exit(par(old.par))
	}

	# save report to a file
	if (saveReport) {
			fileName <- paste0("covid19-SummaryReport_",Sys.Date(),".txt")
			# sink the output to the file and console (split=T) and include messages and warnings too
			sink(file=fileName,split=TRUE) #,type = "message")
			#file.out <- file(fileName, open = "wt")
			#sink(file.out, type = "message")
			#sink(file.out, split=TRUE)
	}

	##### PROCESS TIME SERIES DATA ######
	if ( (toupper(cases.to.process)=="ALL") | (toupper(cases.to.process)=="TS") ) {
	# first column with cases data
	col1 <- 5

	#cases <- c("confirmed","recovered","deaths")
	cases <- c("ts-confirmed","ts-deaths","ts-recovered")	#, "aggregated")

	TS.totals <- list(tots=c(),avgs=c(),sds=c())
	n0 <- c()
	for (i in cases) {
		# read data
		data <- covid19.data(i)

		# run integrity and consistency checks...
		data.checks(data,datasetName=i,details=FALSE,disclose=FALSE)

		# >> nullify data
		#data <- nullify.data(data, stringent=TRUE)

		# if Nentries is set to 0, will consider *all* entries
		if (Nentries==0) Nentriex <- nrow(data)

                colN <- ncol(data)
                nRecords <- nrow(data)
#               if (grepl("aggregated",i))      col.Tgt <-

                # get total Global for confirmed cases
		total.global <- sum(data[,colN],na.rm=TRUE)

		# reporting only for geo.loc indicated
		if (!is.null(geo.loc)) {
			# check the location indicated
			geo.loc <- checkGeoLoc(data,geo.loc)
			# selecting data and updating entries
			data <- select.per.loc(data,geo.loc)
			Nentriex <- min(Nentriex,nrow(data))
		}
		###

		colN <- ncol(data)
		nRecords <- nrow(data)

                # get totals per case
                TS.totals$tots <- c(TS.totals$tots, sum(data[,colN],na.rm=TRUE))
                TS.totals$avgs <- c(TS.totals$avgs, mean(data[,colN],na.rm=TRUE))
                TS.totals$sds <-  c(TS.totals$sds , sd(data[,colN],na.rm=TRUE))
                ###

		n0 <- c(n0,nrow(data))

		# indentify country and province columns
		country.col <- pmatch("Country", names(data))
		province.col <- pmatch("Province", names(data))

		# display 'header'
		report.title <- intro.header(i, data,colN,country.col,province.col, geo.loc)

		# Totals per countries/cities
		#data$Totals <- apply(data[,col1:colN],MARGIN=1,sum)
		data$Totals <- data[,colN]

		# store total nbr of cases
		if (grepl("confirmed",i)) {
			geo.list <- data[,c(country.col,province.col)]
                        total.cases <- data[,colN]
			colsF <- (colN+1):(colN+8)
			total.rel <- sum(total.cases)
			if (!is.null(geo.loc)) {
				data$RelPerc <- round((total.cases/total.rel)*100,2)
				colsF <- (colN+1):(colN+9)
			}
			data$GlobalPerc <- round((total.cases/total.global)*100,2)
		} else {
			# check that the countries/regions match in order to compute percentages...
			# Could improve partial matches...
			if ( sum(as.character(data[,country.col])==as.character(geo.list[1:nRecords,1]))==nRecords
				&&  sum(as.character(data[,province.col])==as.character(geo.list[1:nRecords,2]))==nRecords ) {
				# get percentage cases
				data$Perc  <- round((data[,colN]/total.cases[1:nRecords])*100,2)
				colsF <- (colN+1):(colN+8)
			} else {
				colsF <- (colN+1):(colN+7)
			}
		}
		# last day change for all cases
		data$"LastDayChange" <- data[,colN]-data[,colN-1]
		data$"t-2" <- data[,colN-1]-data[,colN-2]
		#data$"T-2" <- data[,colN]-data[,colN-2]
		data$"t-3" <- data[,colN-2]-data[,colN-3]
		data$"t-7" <- data[,colN-6]-data[,colN-7]
		data$"t-14" <- data[,colN-13]-data[,colN-14]
		data$"t-30" <- data[,colN-29]-data[,colN-30]

		# top countries/regions
		upto.record <- min(nrow(data),Nentriex)
		data.ordered <- data[order(data$Totals,decreasing=TRUE, na.last=NA),][1:upto.record,c(country.col,province.col,colsF)]
		row.names(data.ordered) <- 1:upto.record
		names(data.ordered)[1:3] <- c("Country.Region","Province.State","Totals")
	
		print(data.ordered)

		# Report Average Percentages...
		header('-')
		report.Avgs(data,"GlobalPerc",descr="Global Perc. Average: ")
		report.Avgs(data.ordered,"GlobalPerc",descr=paste("Global Perc. Average in top ",Nentriex,": "))
		header('-')

		header("=")

		# graphics
		if (graphical.output) {
			# check if there are any records with positive values
			if (sum(data.ordered$Totals>0) > 0) {
			legends <- paste(data.ordered$Country.Region,data.ordered$Province.State,'\n',data.ordered$Totals)
			color.scheme <- heat.colors(Nentriex)	#topo.colors(Nentriex)
					#terrain.colors(Nentriex)	#rainbow(Nentriex)

			par(mfrow=c(1,2))
			#par(mfrow=c(1,1))
			pie(na.omit(data.ordered$Totals[data.ordered$Totals>0]),
				labels=legends,
				main=substr(report.title,1,floor(nchar(report.title)/2)),
				col=color.scheme )

			#par(new=TRUE)
			#par(mfrow=c(1,2))
			barplot(data.ordered$Totals, names.arg=legends,
				col=color.scheme,
				main=substr(report.title,ceiling(nchar(report.title)/2),nchar(report.title)) )
			} else {
				warning("Graphical Output: Data yields non-positive results!")
			}
		}
	}

	}

	##### PROCESS "AGREGATED" DATA  #######
	if ( (toupper(cases.to.process)=="ALL") | (toupper(cases.to.process)=="AGG") ) {
		agg.data <- covid19.data("aggregated")

		# >> nullify data
		agg.data <- nullify.data(agg.data)
		process.agg.cases(agg.data, Nentries, geo.loc=geo.loc, graphical.output)

		# report integrity and consistency checks in the data
		integrity.check(agg.data,recommend=FALSE)
	}


	#### OVERALL SUMMARY
	if (!is.null(geo.loc)) {
		report.title <- paste("Time Series ",paste(geo.loc,collapse=','))
	} else {
		report.title <- "Time Series Worldwide"
	}
	if ( (toupper(cases.to.process)=="ALL") | (toupper(cases.to.process)=="TS") ) {
	        report.Totals(cases,TS.totals, preTitle=report.title,n0)	#nrow(data))
	}

	if (saveReport) {
		sink()
		message("Report saved in ",fileName)
	}

	# reset options to user ones
	options(old.opts)

	#return(TS.totals)
}

#############################################################################


single.trend <- function(ts.data, confBnd=TRUE, info="") {
#' function to visualize different indicators for trends in daily changes of cases reported as time series data
#' 
#' @param  ts.data  time series data
#' @param  confBnd  optional argument to remove the drawing of a confidence band
#' @param  info  addtional information to display in plots
#'
#'
#' @importFrom  graphics  axis.Date lines mtext
#'
#' @export
#'
#' @examples
#' \donttest{
#' tor.data <- covid19.Toronto.data()
#' single.trend(tor.data[tor.data$status=="Active Cases",]) 
#' }
#'
#' ts.data <- covid19.data("ts-confirmed")
#' ont.data <- ts.data[ ts.data$Province.State == "Ontario",]
#' single.trend(ont.data)
#'
#' single.trend(ts.data[ ts.data$Country.Region=="Italy",])


        # check that the data is time series
        chk.TS.data(ts.data,xtp=TRUE)

        # first column with daily data
        col1 <- 5
	Ncols <- ncol(ts.data)

	cty.col <- pmatch("Country",names(ts.data))
	prv.col <- pmatch("Province",names(ts.data))
	lat.col <- pmatch("Lat",names(ts.data))
	lng.col <- pmatch("Long",names(ts.data))

	cols.range <- col1:Ncols

	if (nrow(ts.data)>1) {
		#ts.data <- cbind(unique(as.character(ts.data[,c(cty.col)])),
		#		paste(ts.data[,prv.col],collapse="/"),
		#		rbind(apply(ts.data[,c(lat.col,lng.col)],MARGIN=2,mean)),
		#		rbind(apply(ts.data[,col1:Ncols],MARGIN=2,sum)) )

		#ts.data[,5:Ncols] <- as.numeric(ts.data[,5:Ncols])
		ts.data <- apply(ts.data[,col1:Ncols],MARGIN=2,sum)
		cols.range <- 1:length(ts.data)
	}

	xvar <- as.Date(names(ts.data)[cols.range])
	xvar.diff <- xvar[-length(xvar)]
	yvar  <- as.numeric(ts.data[cols.range])
	yvar.diff <- diff(yvar,lag=1)


	# preserve user graphical environment
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))


	### MAIN PLOT
	# plot diff
	plot( xvar.diff,yvar.diff, type='b', cex=0.5,
		#ylim=c(min(yvar.diff),1.15*max(yvar.diff)),
		axes=FALSE, xlab="Time (dates)", ylab="Daily Changes" )
	mtext(info,side=3,padj=0,adj=0)
	axis.Date(1,xvar.diff)
	axis(side=2)

	if (confBnd)
		confBand(xvar.diff,yvar.diff, 1,length(yvar.diff),0,max(yvar.diff,na.rm=TRUE), windowsNbr=10, lcolour='black',lwidth=0.75)


	### SUBPLOTS
	par(new=TRUE)
	par(mfrow=c(4,4), mai = c(0.3, 0.1, 0.1, 0.3))
	par(mfg=c(1,2))

	# plot cumulative nbr of cases
	plot(xvar,yvar, type='l', col='darkblue', xlab='',ylab='Nbr of Cases')
#	if (confBnd)
#		confBand(xvar,yvar, 1,length(yvar),0,max(yvar,na.rm=TRUE), windowsNbr=10, lcolour='black', lwidth=0.5)
	par(new=TRUE)
	plot(xvar,log1p(yvar), type='l', col='blue', lty='dotdash', axes=F, xlab='',ylab='')
	axis(4)

	# plot diff vs total nbr of cases
	par(new=FALSE)
	par(mfg=c(1,3))
	plot(yvar.diff ~ yvar[-length(yvar)], type='l', lwd=0.35,  axes=F, xlab='',ylab='')
	axis(1); axis(4)
	if (confBnd)
		confBand(yvar[-length(yvar)],yvar.diff, 1,length(yvar.diff),0,max(yvar.diff,na.rm=TRUE), windowsNbr=10, lcolour='black',lwidth=0.75)

	### 
	plot( log1p(yvar.diff) ~ log1p(yvar[-length(yvar)]),
			type='p', cex=0.35, pch=20, lwd=.25,  axes=F, xlab='',ylab='',
			xlim=c(0,max(log1p(yvar),log1p(yvar.diff), na.rm=TRUE)) ,
			ylim=c(0,max(log1p(yvar),log1p(yvar.diff), na.rm=TRUE))	)
	lines(log1p(yvar.diff) ~ log1p(yvar[-length(yvar)]), lwd=.25)
	confBand(log1p(yvar[-length(yvar)]),log1p(yvar.diff), .1,length(yvar.diff),0,max(log1p(yvar.diff),na.rm=TRUE), windowsNbr=10, lcolour='black',lwidth=.75)
	abline(0,1, lty=4)
	axis(side=1); axis(side=4)

	# GROWTH RATE
	par(mfg=c(2,2))
	gr.rate <- (yvar.diff[2:length(yvar.diff)]/yvar.diff[1:(length(yvar.diff)-1)])
	#plot(xvar.diff,gr.rate, type='l', ylab="Growth rate", lwd=0.75)
	#confBand(xvar.diff,gr.rate, .5,length(gr.rate),0,max(gr.rate,na.rm=TRUE), windowsNbr=10, lcolour='black')
	par(mfrow=c(6,4), mai=c(0,0,0,0))
	par(mfg=c(3,2))
	mask.data <- which(!is.nan(gr.rate) & !is.infinite(gr.rate) & gr.rate>0)
	gr.rate <- gr.rate[mask.data]

	# will draw plots if there is enough data points...
	if (length(gr.rate)>1) {
		barplot(gr.rate)
		#axis.Date(1,xvar.diff)

	# Normalized growth rate
		par(mfrow=c(15,4), mai=c(0,0,0,0))
		par(mfg=c(5,2))
		norm.gr.rate <- gr.rate/max(gr.rate)

		norm.gr.rate <- (norm.gr.rate)
		plot(xvar.diff[mask.data],norm.gr.rate, axes=FALSE, type='s', lwd=.3)
		#axis.Date(1,xvar.diff[mask.data])
		axis(4)
		confBand(xvar.diff[mask.data],norm.gr.rate, .25,length(norm.gr.rate),0,max(norm.gr.rate,na.rm=TRUE), windowsNbr=15, lcolour='black',lwidth=0.75)
	}
}

#############################################################################

mtrends <- function(data, geo.loc=NULL, confBnd=TRUE, info="") {
#' function to visualize different indicators for trends in daily changes of cases reported as time series data, for mutliple (or single) locations
#'
#' @param  data  data.frame with *time series* data from covid19
#' @param  geo.loc  list of locations
#' @param  confBnd  flag to activate/deactivate drawing of confidence bands base on a moving average window
#' @param  info  additional info to display in the plot
#'
#' @export
#'
#' @importFrom  graphics   barplot par plot abline axis 
#'
#' @examples
#' ts.data <- covid19.data("ts-confirmed")
#' mtrends(ts.data, geo.loc=c("Canada","Ontario","Uruguay","Italy"))
#'

        # check that the data is time series
        chk.TS.data(data,xtp=TRUE)

        # check the location indicated
        geo.loc <- checkGeoLoc(data,geo.loc)

	results <- list()

	# Process each case if status is present...
        if ("status" %in% names(data)) {
                for (i in unique(data$status) ) {
                        message("Considering ",i, " cases")
                        datai <- data[ data$status == i, ]
                        datai <- datai[, ! names(data) %in% "status", drop = F]
                        result.per.case <- mtrends(datai,geo.loc,confBnd, i)
                        results <- list(result.per.case, results)
                }
                return(results)
        }

        for (i in geo.loc) {
                # check whether the locations are countries/regions or provinces/states
                cases.per.loc <- select.per.loc(data,i)

		Country.col <- pmatch("Country", names(data))
		Province.col <- pmatch("Province", names(data))

		colN <- ncol(cases.per.loc)
		if (tolower("status") %in% tolower(cases.per.loc))
			colN <- colN - 1

		for (j in 1:nrow(cases.per.loc)) {
			locn <- paste(cases.per.loc[j,Country.col],cases.per.loc[j,Province.col],collapse=" ")
			header('',paste("Processing ",locn,"..."))
			single.trend(cases.per.loc[j,],confBnd,paste(locn,info))
		}
	}
}

#############################################################################

