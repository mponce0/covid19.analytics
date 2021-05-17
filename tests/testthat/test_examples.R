# TESTS SET for the covid19.analytics package
# M.Ponce


#############################################
############  DATA  RETRIEVAL  ##############
#############################################
test_that("Data Retrieval", {

	loc.data <- c(F,T)

	for (repo in loc.data) {
		# WORLD DATA
		# aggregated
		agg.data <- covid19.data("aggregated", local.data=repo)
		expect_s3_class(agg.data,"data.frame")

		# TS
		ts.data <- covid19.data("ts-ALL", local.data=repo)
		expect_s3_class(ts.data,"data.frame")

		# ALL
		all.data <- covid19.data("ALL", local.data=repo)
		expect_type(all.data,"list")
	}

	# Toronto Data
        # original data type
        expect_type(covid19.Toronto.data(origin="city",data.fmt="original"), "list")

        # TS Toronto data
	# failing on WINDOWS ##i386
	#tor.data <- covid19.Toronto.data()
	#expect_s3_class(tor.data,"data.frame")

	## failing on WINDOWS -- R 3.6.3
	tor.data <- covid19.Toronto.data(local.data=TRUE)
        expect_s3_class(tor.data,"data.frame")

	# US data
	us.data <- covid19.US.data()
	expect_s3_class(us.data,"data.frame")
})


#####################################################################

### Toronto Data

test_that("Toronto Data", {
	# TS Toronto data
	# failing on WINDOWS ##i386
	#tor.data <- covid19.Toronto.data()
	#expect_s3_class(tor.data,"data.frame")

	# failing on WINDOWS -- R 3.6.3
	tor.data <- covid19.Toronto.data(local.data=TRUE)
	expect_s3_class(tor.data,"data.frame")

	# TRENDS
	# single trend
	st.Tor <- unique(tor.data$status)
	for (i in st.Tor) {
		expect_warning(single.trend(tor.data[tor.data$status == i,], info=i))
	}

	# Multiple trends
	expect_output(mtrends(tor.data))
})


#####################################################################

### Integrity/Consistencies tests

test_that("integrity/consistency", {
	source("covid19_checks_tests.R")
	expect_warning(test.data.checks())
})

#####################################################################

##### CONTIONAL TESTS ####### ==> to reduce testing time
# flag to control how many test need to be run...
# using *either*
#	an OS enviroment variable COVID19ANALYTICS = "full.test"
# OR
#	an R variable covid19analytics.testing = "full.test"

# check OS environment variable
c19fulltest_OS <- Sys.getenv("COVID19ANALYTICS")=="full.test"
# check R enviroment variable
c19fulltest_R <- ( ("covid19analytics.testing" %in% ls()) && (covid19analytics.testing == "full.test") )

message(c19fulltest_OS)
message(c19fulltest_R)

#c19fulltest_R <- TRUE
#assign("N",tot.population, envir = .SIR.model.env)	#.GlobalEnv)

# check if the fulltest flags were activated...
if (c19fulltest_OS | c19fulltest_R) {

	message("Running FULL batery of TEST for covid19.analytics...")

### TOTS.PER.LOCATION()

test_that("TOTS.PER.LOCATION fn", {
	cases <- c("confirmed","deaths","recovered")

	for (i in seq_along(cases)) {
		i.case <- cases[i]
		header("",paste('processing ',i.case))

		# read the time series data for confirmed cases
		all.data <- covid19.data(paste0('ts-',i.case))
		expect_s3_class(all.data,"data.frame")

		# run on all the cases
		#pdf(paste0("Japan_",i.case,".pdf"))
		expect_invisible(tots.per.location(all.data,"Japan"))
		#dev.off()
	}
})


### GROWTH.RATE()
test_that("GROWTH.RATE fn", {
	# read time series data for confirmed cases
	TS.data <- covid19.data("ts-confirmed")
	expect_s3_class(TS.data,"data.frame")

	locs <- c("Hubei","Italy","Germany","Canada")
	for (i in seq_along(locs)) {
		i.loc <- locs[i]

		print(i.loc)

		# compute changes and growth rates per location for 'Italy'
		#pdf(paste0("gr-changes_",i.loc,".pdf"))
		expect_output( growth.rate(TS.data,geo.loc=i.loc) )
		#dev.off()

	}
})


test_that("GROWTH.RATE fn with more cases...", {
	# obtain Time Series data
	TSconfirmed <- covid19.data("ts-confirmed")

	# explore different combinations of regions/cities/countries
	# when combining different locations heatmaps will also be generated comparing the trends among these locations
	expect_output( growth.rate(TSconfirmed,geo.loc=c("Italy","Canada","Ontario","Quebec","Uruguay")) )

	expect_output( growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","United States","Canada","Ontario","Quebec","Uruguay")) )

	expect_output( growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","US","Canada","Ontario","Quebec","Uruguay")) )
})


##########################################################################


########### REPORTS ##############

test_that("REPORTS ...", {
	expect_output(report.summary(graphical.output=FALSE))
	
	expect_output(report.summary(graphical.output=FALSE, saveReport=TRUE) )
})

###########################################################################

#### INTERACTIVE PLOTS
# retrieve time series data
TS.data <- covid19.data("ts-ALL")

# static and interactive plot 
totals.plt(TS.data, interactive.display=FALSE)


# retrieve aggregated data
data <- covid19.data("aggregated")

# interactive map of aggregated cases -- with more spatial resolution
live.map(data, interactive.display=FALSE)

# or
live.map(interactive.display=FALSE)

# interactive map of the time series data of the confirmed cases with less spatial resolution, ie. aggregated by country
live.map(covid19.data("ts-confirmed"), interactive.display=FALSE)



#### SIR MODEL
# read time series data for confirmed cases
data <- covid19.data("ts-confirmed")

# run a SIR model for a given geographical location
generate.SIR.model(data,"Hubei", t0=1,t1=15)
generate.SIR.model(data,"Germany",tot.population=83149300)
generate.SIR.model(data,"Uruguay", tot.population=3500000)
generate.SIR.model(data,"Ontario",tot.population=14570000)

# the function will agregate data for a geographical location, like a country with multiple entries
generate.SIR.model(data,"Canada",tot.population=37590000)

# modelling the spread for the whole world, storing the model and generating an interactive visualization
world.SIR.model <- generate.SIR.model(data,"ALL", t0=1,t1=15, tot.population=7.8e9, staticPlt=FALSE)
# plotting and visualizing the model
plt.SIR.model(world.SIR.model,"World",interactiveFig=FALSE,fileName="world.SIR.model")


}

####
# clean up
# use a variable "covid19analytics.test.DONT.cleanup" to don't delete files

#if(!("covid19analytics.test.DONT.cleanup" %in% ls())) {
if (FALSE) {
	rm.file.tps <- c("txt","pdf") 
	for (tp in rm.file.tps)
		for (fl in dir(pattern=tp)) {
			message("clean-up: removing",fl)
			file.remove(fl)
		}
}

####
