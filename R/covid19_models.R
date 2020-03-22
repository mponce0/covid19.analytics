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
				t0=30,t1=45, tfinal=90,
				fatality.rate = 0.02,
				tot.population=1400000000) {
#' function to generate a simple SIR model
#'
#' @param  data  dataset to consider
#' @param  geo.loc  country/region to analyze
#' @param  tfinal  total number of days
#' @param  tot.population  total population of the country/region
#'
#' @importFrom  stats  optim setNames
#' @importFrom  deSolve  ode
#' @importFrom  graphics  matplot title legend points
#'
#' @export
#'

	header <- function(x="-",title="",total.len=80,eol='\n') {
		len.title <- nchar(title)
		if (len.title !=0) {
			reps <- (total.len-len.title)/2
			header(x,total.len=reps,eol=' ')
			cat(title)
			header(x,total.len=reps,eol='\n')
		} else {
			cat(paste(paste(rep(as.character(x),total.len),collapse=""),eol))
		}
	}

	header("#")
	message("This is an experimental feature, being currently under active development!")
	message("Please check the development version of the package for the latest updates on it")
	header("#")

	header('-'," Parameters used to create model ")
	cat('\t',"Region: ",geo.loc,'\n')
	cat('\t',"Time interval to consider: t0=",t0," - t1=",t1," ; tfinal=",tfinal,'\n')
	cat('\t',"Fatality rate: ",fatality.rate,'\n')
	cat('\t',"Population of the region: ",tot.population,'\n')
	header('-')

	# get actual data from indicated region...
	Infected <- preProcessingData(data,geo.loc)
	Infected <- Infected[cumsum(Infected)!=0]
	Infected <- Infected[t0:t1]
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

	col <- 1:3 # colour
 
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
	cat("Max of infecteded:")
	print(fit[fit$I == max(fit$I), "I", drop = FALSE]) 

	# Max number of casualties
	cat("Max nbr of casualties, with ", fatality.rate*100,"% fatality rate:")
	print(max(fit$I) * fatality.rate)
}

#######################################################################
#######################################################################

#######################################################################

