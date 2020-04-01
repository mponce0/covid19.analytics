# Module to define object class for reporting information to the user
#
# M.Ponce


#############################################################################

# Define methods associated with descrn.obj
#print <- function(report.obj){
##' method associated with 'report' objects
##' @param  report.obj  'report' object
##' @keywords internal
#       UseMethod("print",report.obj)
#}

print.report <- function(report.obj) {
#' function associated to the 'report' object method
#' @param  report.obj  'report' object
#' @keywords internal

	cat(report.obj)
}



#############################################################################

header <- function(x="-",title="",total.len=80,eol='\n') {
#' auxiliary fn to print "headers" adn 'titles'
#'
#' @param  x  character to use as lines
#' @param  title  title to dispo
#' @param  total.len  length of the line
#' @param  eol  end of line character
#'
#' @keywords internal
#'

	# set object type
	class(title) <- "report.obj"

        len.title <- nchar(title)
        if (len.title !=0) {
                reps <- (total.len-len.title)/2
                if (reps < 0) reps <- 1
                header(x,total.len=reps,eol=' ')
                print.report(title)
                header(x,total.len=reps,eol='\n')
        } else {
                line.rep <- paste(paste(rep(as.character(x),total.len),collapse=""),eol)
		class(line.rep) <- "report.obj"
		print.report(line.rep)
        }
}

#############################################################################
