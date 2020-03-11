totals <- function() {

	total.cases <- covid19()
	col1 <-5; colN <- ncol(total.cases)-1

	totals <- apply(total.cases[,col1:colN], MARGIN=2,sum)

	categories <- unique(total.cases$status)
	totals.per.cat <- c()
	for (categ in seq_along(categories)) {
		totals.per.cat[categ] <- apply(total.cases[total.cases$status==categ,col1:colN], MARGIN=2,sum)
	}
	confirmed <- apply(total.cases[total.cases$status=="confirmed",col1:colN], MARGIN=2,sum)
	recovered <- apply(total.cases[total.cases$status=="recovered",col1:colN], MARGIN=2,sum)
	deaths <- apply(total.cases[total.cases$status=="death",col1:colN], MARGIN=2,sum)

	x.dates <- as.Date(names(total.cases)[col1:colN])
	ymax <- max(totals)

	### STATIC PLOTS
	plot(x.dates, totals, ylim=c(0,ymax),
		xlab='time', ylab='nbr of cases', type='b', col='darkred')

	par(new=TRUE)
	plot(x.dates,confirmed, ylim=c(0,ymax), axes=FALSE, type='l', col='black')
	plot(x.dates,recovered, ylim=c(0,ymax), axes=FALSE, type='l', col='blue')
	plot(x.dates,deaths, ylim=c(0,ymax), axes=FALSE, type='l', col='red')

	### INTERACTIVE PLOTS
	fig <- plot_ly(data, x = ~colnames(data[,col1:colN]))	#, type='scatter', mode='line+markers')
#	for (categ in categories) {
#		fig <- fig %>% add_trace(y = ~categ, name="confirmed", mode='line+markers')
#	}
	fig <- fig %>% add_trace(y = ~confirmed, name="confirmed", type='scatter', mode='lines+markers')
	fig <- fig %>% add_trace(y = ~recovered, name="recovered", type='scatter', mode='lines+markers')
	fig <- fig %>% add_trace(y = ~deaths, name="deaths", type='scatter', mode='lines+markers')
}



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
