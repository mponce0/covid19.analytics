# Modelling fns of the covid19 package
#
# M.Ponce


#######################################################################

genModel <- function(y, deg=1) {
#' function to generate models using Linear Regression "LM"
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

	cat("Linear Regression (lm):", '\n')
	print(summary(model))

	return(model)	
}

evalModel <- function(model) {

}


#######################################################################

gen.glm.model <- function (y, family=Gamma(link="log")) {
#' function to generate models using GLM
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

	cat(paste0("GLM using Family ",
		paste(capture.output(eval(family)),collapse="")," :"),'\n')
	print(summary(model))

	return(model)
}

#######################################################################
#######################################################################
##### UNDER DEVELOPMENT #####
#######################################################################
#######################################################################

simple.SIR.ODE <- function(time, state, parameters) {
# Define ODE for SIR model


  ODE.params <- as.list(c(state, parameters))

  # define ODE
  with(ODE.params, {
    dSdt <- -beta/N * I * S
    dIdt <- beta/N * I * S - gamma * I
    dRdt <- gamma * I
    list(c(dSdt, dIdt, dRdt))
    })
}


simple.SIR.solver <- function(data=NULL, geo.loc="Hubei",
				t0=NULL,t1=NULL, deltaT=NULL,
				tfinal=90,
				fatality.rate = 0.02,
				tot.population=1400000000) {
#' function to generate a simple SIR model
#'
#' @param  data  dataset to consider
#' @param  geo.loc  country/region to analyze
#' @param  t0  initial period of time for data consideration
#' @param  t1  final period of time for data consideration
#' @param  deltaT interval period of time from t0, ie. number of days to consider since t0
#' @param  tfinal  total number of days
#' @param  fatality.rate  rate of causality, deafault value of 2%
#' @param  tot.population  total population of the country/region
#'
#' @importFrom  stats  optim setNames
#' @importFrom  deSolve  ode
#' @importFrom  graphics  matplot title legend points
#'
#' @export
#'
#' @examples
#' data <- covid19.data("confirmed")
#' simple.SIR.solver(data,"Hubei")
#' simple.SIR.solver(data,"Germany",tot.population=83149300)
#' simple.SIR.solver(data,"Uruguay", tot.population=3500000)
#'

	# DISCLAIMER // EXPERIMENTAL FEATURES
	header("#")
	message("This is an experimental feature, being currently under active development!")
	message("Please check the development version of the package for the latest updates on it")
	header("#")


	# get actual data from indicated region...
	pot.Infected <- preProcessingData(data,geo.loc)
	# eliminate entries without cases, ie equal 0
	#pot.Infected <- pot.Infected[cumsum(pot.Infected)!=0]
	print(pot.Infected)


        # default values
        t0.default=30
        deltaT.default=25
        t1.default=t0.default+deltaT.default
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
		print(Infected)
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
	cat('\t',"Region: ",geo.loc,'\n')
	cat('\t',"Time interval to consider: t0=",t0," - t1=",t1," ; tfinal=",tfinal,'\n')
	cat('\t\t',"t0: ",names(data)[t0]," -- t1: ",names(data)[t1],'\n')
	cat('\t',"Number of days considered for initial guess: ",length(Infected),'\n')
	cat('\t',"Fatality rate: ",fatality.rate,'\n')
	cat('\t',"Population of the region: ",tot.population,'\n')
	header('-')
	###########

	print(Infected)

	# total population of the region
	N <<- tot.population


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

	old <- par(mfrow = c(2, 2))
	plot(Day, Infected, type ="b")
	plot(Day, Infected, log = "y")
	abline(lm(log10(Infected) ~ Day))
	title(paste("Confirmed Cases 2019-nCoV:",geo.loc), outer = TRUE, line = -2)

	loadLibrary("deSolve")
	init.cond <- c(S = N-Infected[1], I = Infected[1], R = 0)

	# define RSS fn to measure deviation of the model from data....
	RSS <- function(parameters) {
		names(parameters) <- c("beta", "gamma")
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

	# definte integration interval... 
	time <- 1:tfinal # time in days
	fit <- data.frame(ode(y = init.cond, times = time, func = simple.SIR.ODE, parms = Opt_par))

	col <- c("blue","red","green") 
 
	matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
	matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
	## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
	## omitted from logarithmic plot

	points(Day, Infected)
	legend("bottomright", c("Susceptible", "Infected", "Recovered"), lty = 1, lwd = 2, col = col, inset = 0.05)
	title(paste("SIR model 2019-nCoV:", geo.loc), outer = TRUE, line = -22)

	# R_naught
	R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
	print(R0)
 
	# height of pandemic
	max.I <- max(fit$I)
	max.I.time <- fit$time[fit$I==max.I]
	cat("Max of infecteded:", max.I, " (",(max.I/tot.population)*100,"%)",'\n')
	#print(fit[fit$I == max(fit$I), "I", drop = FALSE]) 

	# Max number of casualties
	cat("Max nbr of casualties, with ", paste0(fatality.rate*100,"% fatality rate:"),
		max.I*fatality.rate,'\n')
	cat("Max reached at day :", max.I.time,
		'==> ')
	print(as.Date(names(data)[t0])+max.I.time)

	header("=")
}

#######################################################################
#######################################################################

#######################################################################

