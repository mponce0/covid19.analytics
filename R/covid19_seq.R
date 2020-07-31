# module for reading covid19.analytics RNA sequence
#
# M.Ponce

X.covid19.genomic.data <- function(graphics.ON=TRUE) {
#' function to obtain sequencing data grom NCBI
#' Reference:  https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2
#' 
#' @param  graphics.ON  flag to activate/deactivate graphical output
#'
#' @importFrom  ape  read.GenBank
#
#' @export
#'
#' @examples
#' # obtain covid19's genomic data
#' covid19.gen.seq <- covid19.genomic.data()
#' # display the actual RNA seq
#' covid19.gen.seq$NC_045512.2
#'

	# load 'ape' library
	loadLibrary("ape")

	# retrieve genetic data
	message("Retrieving data from NCBI...")
	covid19.seq <- read.GenBank("NC_045512.2",as.character=TRUE)

	print(summary(covid19.seq))

	if (graphics.ON) {
		freq.table.ACGT <- table(covid19.seq$NC_045512.2)
		bplt <- barplot(freq.table.ACGT/sum(freq.table.ACGT),
				col=heat.colors(4),
				main="ACTG Distribution in covid19 genome")
		#text(names(freq.table.ACGT),bplt+3,freq.table.ACGT,xpd=TRUE, col='blue')
	}

	return(covid19.seq)
}
