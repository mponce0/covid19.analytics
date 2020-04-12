# Modelling fns of the covid19 package
#
# M.Ponce


#######################################################################

genModel <- function(y, deg=1) {
#' function to generate models using Linear Regression "LM"
#'
#' @param  y  vector containing the data to fit
#' @param  deg  degree of the polynomial fit
#' 
#' @keywords internal
#'
#' @importFrom  stats  lm
#'

	y.var <- unlist(y)
	x.var <- 1:length(y.var)
	#print(x.var)
	#print(y.var)

	model <- lm(y.var ~ x.var)

	#header('-')
	header('',"Linear Regression (lm):")
	print(summary(model))
	header('-')

	return(model)	
}

evalModel <- function(model) {

}


#######################################################################

gen.glm.model <- function (y, family=Gamma(link="log")) {
#' function to generate models using GLM
#'
#' @param  y  vector containing the data to fit
#' @param  family  family to use in the GLM method
#'
#' @keywords internal
#'
#' @importFrom  stats  glm  Gamma poisson
#' @importFrom  utils  capture.output
#'

	y.var <- unlist(y)
	x.var <- 1:length(y.var)
	#print(x.var)
	#print(y.var)

	model <- glm(y.var ~ x.var, family=family)

	#header('-')
	header('',paste0("GLM using Family ",
		paste(capture.output(eval(family)),collapse="")," :") )
	print(summary(model))
	header('-')

	return(model)
}

#######################################################################
#######################################################################
##### UNDER ACTIVE DEVELOPMENT #####
#######################################################################
#######################################################################

simple.SIR.ODE <- function(time, state, parameters) {
#' Define an ODE system for a simple SIR model
#'
#' @param  time  time variable
#' @param  state  state variable
#' @param  parameters parameters of the ODE
#'
#' @keywords internal
#'

  ODE.params <- as.list(c(state, parameters))

  # define ODE
  with(ODE.params, {
    dSdt <- -beta/N * I * S
    dIdt <- beta/N * I * S - gamma * I
    dRdt <- gamma * I
    list(c(dSdt, dIdt, dRdt))
    })
}


generate.SIR.model <- function(data=NULL, geo.loc="Hubei",
				t0=NULL,t1=NULL, deltaT=NULL,
				tfinal=90,
				fatality.rate = 0.02,
				tot.population=1400000000,
				staticPlt=TRUE, interactiveFig=FALSE) {
#' function to generate a simple SIR (Susceptible-Infected-Recovered) model based on the actual data of the coivd19 cases
#'
#' @param  data  time series dataset to consider
#' @param  geo.loc  country/region to analyze
#' @param  t0  initial period of time for data consideration
#' @param  t1  final period of time for data consideration
#' @param  deltaT interval period of time from t0, ie. number of days to consider since t0
#' @param  tfinal  total number of days
#' @param  fatality.rate  rate of causality, deafault value of 2 percent
#' @param  tot.population  total population of the country/region
#' @param  staticPlt  optional flag to activate/deactive plotting of the data and the SIR model generated
#' @param  interactiveFig  optional flag to activate/deactive the generation of an interactive plot of the data and the SIR model generated
#'
#' @importFrom  stats  optim setNames
#' @importFrom  deSolve  ode
#'
#' @export
#'
#' @examples
#' data <- covid19.data("ts-confirmed")
#' generate.SIR.model(data,"Hubei", t0=1,t1=15)
#' generate.SIR.model(data,"Germany",tot.population=83149300)
#' generate.SIR.model(data,"Uruguay", tot.population=3500000)
#' generate.SIR.model(data,"Canada", tot.population=37590000)
#'

	# DISCLAIMER // EXPERIMENTAL FEATURES
	header("#")
	message("This is an experimental feature, being currently under active development!")
	message("Please check the development version of the package for the latest updates on it")
	header("#")


	# check that the data is time series data
	chk.TS.data(data,xtp=TRUE)

	# get actual data from indicated region...
	pot.Infected <- preProcessingData(data,geo.loc)
	# eliminate entries without cases, ie equal 0
	#pot.Infected <- pot.Infected[cumsum(pot.Infected)!=0]
	#print(pot.Infected)


        # default values
        t0.default=30
        deltaT.default=25
        t1.default=t0.default+deltaT.default
	#
	colOffset <- 5
        ###########
	
	if (is.null(t0)) {
		# attempt to auto-detect significant growth....
		# will use a moving average of 2 days...
		#x <- movingFn(pot.Infected,period=2)
		x <- pot.Infected
		print(x)
		l1 <- length(x)
		l2 <- l1-1
		threashold <- 10
		t0 <- min(which(cumsum( (x[2:l1]-x[1:l2]) > threashold) != 0),na.rm=TRUE)
		print(t0)
		if (!is.null(t1)) {
			if (t1>t0) {
				Infected <- pot.Infected[t0:min(t1,l1)]
			} else {
				Infected <- pot.Infected[t0:min(t0+deltaT.default,l1)]
			}
		} else if (!is.null(deltaT)) {
				Infected <- pot.Infected[t0:min(t0+deltaT,l1)]
			} else {
				Infected <- pot.Infected[t0:min(t0+deltaT.default,l1)]
			}
		#print(Infected)
	} else if (!is.null(t1)) {
			# user indicated t0 & t1
			Infected <- pot.Infected[t0:t1]
		} else if (!is.null(deltaT)) {
				# user indicated t0 & deltaT
				Infected <- pot.Infected[t0:(t0+deltaT)]
			} else {
				Infected <- pot.Infected
			}


#	if (is.null(t0) & is.null(t1) & is.null(deltaT)) {
#		t0 <- t0.default
#		t1 <- t1.default
#	}

	print(Infected)


	###########
	header('-',"Parameters used to create model ")
	header("",paste0('\t',"Region: ",toupper(geo.loc)))
	header("",paste0('\t',"Time interval to consider: t0=",t0," - t1=",t1," ; tfinal=",tfinal))
	header("",paste0('\t\t',"t0: ",names(data)[t0+colOffset]," -- t1: ",names(data)[t1+colOffset]))
	header("",paste0('\t',"Number of days considered for initial guess: ",length(Infected)))
	header("",paste0('\t',"Fatality rate: ",fatality.rate))
	header("",paste0('\t',"Population of the region: ",tot.population))
	header('-')
	###########

	#print(Infected)

	# total population of the region
	# N <<- tot.population
	# needs to be a global variable for the ODE solver
	# will enforce a new environment
	.SIR.model.env <- new.env()
	assign("N",tot.population, envir = .SIR.model.env)	#.GlobalEnv)

	#### TEST CASES ####
	###### MAINLAND CHINA #####
	#Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711, 9692, 11791, 14380, 17205, 20440)
	#N <- 1400000000 # population of mainland china
	###### GERMANY #######
	#Infected <- c(16, 18, 21, 26, 53, 66, 117, 150, 188, 240, 349, 534, 684, 847, 1110, 1458, 1881, 2364, 3057, 3787, 4826, 5999)
	#N <- 83149300 # population of Germany acc. to Destatis
	#######################


	# Determine nbr of days 
	Day <- 1:(length(Infected))

        
	loadLibrary("deSolve")
	init.cond <- c(S = .SIR.model.env$N-Infected[1], I = Infected[1], R = 0)

	# define RSS fn to measure deviation of the model from data....
	RSS <- function(parameters) {
		# include N as a parameter for the ODE
		parameters <- c(parameters,.SIR.model.env$N)
		names(parameters) <- c("beta", "gamma","N")
		out <- ode(y = init.cond, times = Day, func = simple.SIR.ODE, parms = parameters)
		fit <- out[ , 3]
		sum((Infected - fit)^2)
	}
	#######
 

	Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
	print(Opt$message)
	## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
 
	Opt_par <- setNames(Opt$par, c("beta", "gamma"))
	print(Opt_par)
	##      beta     gamma 
	## 0.6746089 0.3253912

	# add N as a parameter for the ODE...
	Opt_par <- c(Opt_par,.SIR.model.env$N)
	names(Opt_par)[3] <- "N"

	# definte integration interval... 
	time <- 1:tfinal # time in days
	fit <- data.frame(ode(y = init.cond, times = time, func = simple.SIR.ODE, parms = Opt_par))

	# R_naught
	R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
	header("",paste("R0 =",R0))
 
	# height of pandemic
	max.I <- max(fit$I)
	max.I.time <- fit$time[fit$I==max.I]
	header("",paste("Max of infecteded:", round(max.I,2), " (",round((max.I/tot.population)*100,2),"%)"))
	#print(fit[fit$I == max(fit$I), "I", drop = FALSE]) 


	# Max number of casualties
	header("",paste("Max nbr of casualties, assuming ", paste0(round(fatality.rate*100,2),"% fatality rate:"),
		round(max.I*fatality.rate,2)))
	header("",paste("Max reached at day :", max.I.time, '==> ',as.Date(names(data)[t0+colOffset])+max.I.time))

	header("=")

	# group fit and data in an object to be returned
	SIR.model <- list(Infected=Infected, model=fit)

	# display plots if requested
	if (staticPlt | interactiveFig)
		plt.SIR.model(SIR.model,geo.loc, interactiveFig)

	#class(SIR.model) <- "SIR.model"
	return(SIR.model)
}

#######################################################################
#######################################################################

plt.SIR.model <- function(SIR.model, geo.loc="", interactiveFig=FALSE, fileName=NULL) {
#' function to plot the results from the SIR model fn
#'
#' @param  SIR.model model resulting from the generate.SIR.model() fn
#' @param  geo.loc  optional string to specify geographical location
#' @param  interactiveFig  optional flag to activate interactive plot
#' @param  fileName  file where to save the HTML version of the interactive figure
#'
#' @export plt.SIR.model 
#'
#' @importFrom  graphics  matplot title legend points
#' @importFrom  plotly  plot_ly %>% add_trace as_widget
#' @importFrom  htmlwidgets  saveWidget
#'

	# recover data from model
	Infected <- SIR.model$Infected
	fit <- SIR.model$model

        # Determine nbr of days 
        Day <- 1:(length(Infected))


        ### preserve user graphical env.
        # save the state of par() before running the code
        oldpar <- par(no.readonly = TRUE)
        # restore the previous state after the fn is done, even if it fails, so the user environment is not altered
        on.exit(par(oldpar))
        #########

        par(mfrow = c(2, 2))
        plot(Day, Infected, type ="b")
        plot(Day, Infected, log = "y")
        abline(lm(log10(Infected) ~ Day))
        title(paste("Confirmed Cases 2019-nCoV:",toupper(geo.loc)), outer = TRUE, line = -2)

        col <- c("blue","red","green")

        matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
        matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
        ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
        ## omitted from logarithmic plot

        points(Day, Infected)
        legend("bottomright", c("Susceptible", "Infected", "Recovered"), lty = 1, lwd = 2, col = col, inset = 0.05)
        title(paste("SIR model 2019-nCoV:", toupper(geo.loc)), outer = TRUE, line = -22)
        #axis.Date(1,as.Date(names(data)[colOffset:ncol(data)]))

        ### INTERACTIVE PLOTS
        if (interactiveFig) {
                # load/check plotly
                loadLibrary("plotly")

                # define interactive figure/plot
		model.ifig <- plot_ly(data=fit, x = ~fit[,1])

		length(Infected) <- length(fit[,1])

		loc.data <- cbind(Infected,fit[,1:4])
		#print(loc.data)

		#model.ifig <- model.ifig %>% add_trace(y = ~Infected, name="Actual data", type='scatter', mode='markers', visible=TRUE)
		# add traces
		model.ifig <- add.N.traces(model.ifig,loc.data, c("data","Susceptible", "Infected", "Recovered"), vis=TRUE)

		# extra traces for activating log-scale
		model.ifig <- add.N.traces(model.ifig, log10(loc.data), c("data","Susceptible", "Infected", "Recovered"), vis=FALSE)
                        
		# log-scale menu based on nbr of traces...
                        
		updatemenues <- log.sc.setup(4)

                # add a menu for switching log/linear scale
                model.ifig <- model.ifig %>% layout(updatemenus=updatemenues)


                # activate interactive figure
                print(model.ifig)


		if (!is.null(fileName)) {
			FileName <- paste0(fileName,".html")
			# informing where the plot is going to be saved
			message("Saving interactive plot in ", FileName)
			htmlwidgets::saveWidget(as_widget(model.ifig), FileName)
		}
	}
}

#######################################################################

