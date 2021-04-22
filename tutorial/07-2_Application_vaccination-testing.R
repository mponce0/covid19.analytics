testVaccCtry <- function(Ctry="Canada"){
   # fn that process testing/vaccination/and/confirmed cases for a given Ctry
   # and generates graphical representations to aid their comparison

   # read DATA
   ## reading COVID19 testing data
   c19.testing.data <- covid19.testing.data()

   ## reading COVID19 vaccination data
   c19.vacc.data <- covid19.vaccination()

   ## reading COVID19 cases
   c19.cases.data <- covid19.data("ts-confirmed")


   # data processing
   ## testing data
   ### select cases for a particular country, given by Ctry
   filter.ctry <- grepl(Ctry,c19.testing.data[,"Entity"])

   ### select columns: date and (short term) positive rate
   cols <- c("Date","Short.term.positive.rate")
   tstDta <- na.omit(c19.testing.data[filter.ctry, cols])
   names(tstDta) <- cols

   ### sort by date
   tstDta <- tstDta[order(tstDta[,1]),]

   ## vaccination data
   ### remove NAs
   vaccs <- na.omit(c19.vacc.data)
   ### select specific "Ctry"
   vacc.Ctry <- vaccs[vaccs$location==Ctry,]

   ## confirmed cases
   conf.Ctry <- c19.cases.data[c19.cases.data$Country.Region==Ctry,5:(length(c19.cases.data)-1)]


   ##### Graphics #####


   # plot daily vaccination per million, for every location
   #par(mfrow=c(5,5))
   #tapply(vaccs$daily_vaccinations_per_million,vaccs$location, plot)
   #par(mfrow=c(1,1))

   ###

   # mosaic plot combining testing/vaccination and confirmed cases data

   par(mfrow=c(3,1))
   par(mar=c(1,5,2,5))

   ### subplot #1
   minX <- as.Date(names(conf.Ctry)[1])
   maxX <- as.Date(names(conf.Ctry)[length(conf.Ctry)])
   # plot positive testing rate vs date
   plot(as.Date(tstDta$Date), tstDta[,2], 'l', ylab="Positive Testing Rate", xlim=c(minX,maxX))
   title(Ctry)
   par(new=TRUE)
   # add vaccination data 
   plot(as.Date(vacc.Ctry$date),(vacc.Ctry$people_vaccinated),
   	type='l', col='blue', xlab=NA, xaxt='n', ylab=NA, yaxt='n', xlim=c(minX,maxX))
   axis(4,col.axis='blue', line=-3.5, las=1, lwd=0)
   #axis.Date(3, as.Date(vacc.Ctry$date), col.axis='blue')
   par(new=TRUE)
   plot(as.Date(tstDta$Date), tstDta[,2], 'l', ylab=NA,yaxt='n', xlim=c(minX,maxX))
   #axis.Date(3, as.Date(vacc.Ctry$date), col.axis='red')
   par(new=TRUE)
   plot(as.Date(names(conf.Ctry)), as.numeric(conf.Ctry), type='s', col='red', ylab=NA,yaxt='n')
   #axis.Date(3, as.Date(vacc.Ctry$date), col.axis='red')
   #axis.Date(1,as.Date(names(conf.Ctry)))	#, at = seq(as.Date(names(conf.Ctry[1])),as.Date(names(conf.Ctry[length(conf.Ctry)]))) )
   axis(4,col.axis='red')
   mtext("Confirmed cases", 4, line=2, cex=.65, col='red')
   legend("top",c("Pos.Testing Rate","Vaccination","Confirmed Cases"),col=c('black','blue','red'),lty=c(1,1,1), bty='n')
   rect(as.Date(vacc.Ctry$date)[1],1, as.Date(vacc.Ctry$date)[length(vacc.Ctry$date)-1], as.numeric(conf.Ctry)[length(conf.Ctry)-1],
	border='darkgray', lty=4, lwd=1.5)

   par(mar=c(1,5,1,5))

   ### subplot #2
   # adjust limits to match testing/vaccination ranges...
   minX <- max(as.Date(tstDta$Date)[1],as.Date(vacc.Ctry$date)[1])
   maxX <- min(as.Date(vacc.Ctry$date)[length(vacc.Ctry$date)], as.Date(tstDta$Date)[length(tstDta$Date)])

   plot(as.Date(tstDta$Date), tstDta[,2], 'l', xlim=c(minX,maxX), xlab=NA,ylab="Positive Testing Rate")
   par(new=TRUE)
   plot(as.Date(names(conf.Ctry)),(as.numeric(conf.Ctry)), xlim=c(minX,maxX), type='l', col='red', xlab=NA,ylab=NA,yaxt='n')
   axis(4, col.axis="red")
   mtext("Confirmed cases", 4, line = 2, cex=.65, col='red')

   par(mar=c(2.5,5,1,5))

   ### subplot 3
   minY=min(vacc.Ctry$people_vaccinated)
   maxY=max(vacc.Ctry$people_vaccinated)
   plot(as.Date(vacc.Ctry$date),(vacc.Ctry$people_vaccinated),
   	xlim=c(minX,maxX), ylim=c(minY,maxY),
   	type='l', col='blue', ylab="Vaccinations")
   axis(2, col.axis='blue', line=-3.5, las=1, lwd=0)

   par(new=TRUE)
   plot(as.Date(vacc.Ctry$date),vacc.Ctry$people_fully_vaccinated,
   		xlim=c(minX,maxX), ylim=c(minY,maxY),
		type='l', col='darkgreen',
   		yaxt = "n", ylab = NA)
   axis(2,col.axis='black')

   par(new=TRUE)
   plot(as.Date(vacc.Ctry$date),vacc.Ctry$daily_vaccinations_per_million,
	xlim=c(minX,maxX),
	type='l', col='blue', lty=2,
   	yaxt = "n", ylab = NA)
   axis(4, col.axis='blue')

   legend("top",c("people vaccinated","fully vaccinated","daily vacc. per M"),col=c('blue','darkgreen','blue'),lty=c(1,1,2), bty='n')

   par(new=FALSE)
   par(mfrow=c(1,1))

}

##########

# Apply the fn to a set of countries...
lapply(c("Argentina","Uruguay","Italy","Spain","Switzerland","New Zealand","Israel"), testVaccCtry)
