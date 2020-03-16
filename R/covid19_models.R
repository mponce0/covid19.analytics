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

