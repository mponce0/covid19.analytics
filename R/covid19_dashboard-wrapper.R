# covid19.analytics Dashboard -- main wrapper function
#
# M.Ponce/A.Sandhel


#######################################################################


covid19Explorer <- function(locn=NULL) {
#' covid19.analytics explorer dashboard
#'
#' @param  locn geographical location to use as default
#'
#' @export
#'

	c19Dashboard <- covid19dashboard(locn=locn)

	ui <- c19Dashboard[[1]]
	server <- c19Dashboard[[2]]

	capture.output(shinyApp(ui = ui, server = server))

}
