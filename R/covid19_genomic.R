# module for reading SARS-CoV-2 genomic data
# part of covid19.analytics package
#
# M.Ponce



############ AUXILIARY FUNCTIONS ############################################


getFile <- function(url=NULL,fileName=NULL) {
                
        if (!is.null(url) & !is.null(fileName)) {
                fileNP <- normalizePath(file.path(tempdir(), fileName), mustWork=FALSE)
                remote.loc <- paste0(url,fileName)
                download.file(remote.loc,fileNP)

                if (!file.exists(fileNP)) {
                        stop("Error download file",fileName,"from ",url,'into',fileNP)
                } else {
                        return(fileNP)
                }
        } else {
                stop("Arguments url and filename must be indicated!")
        }
}


##############


badOption <- function(arg) {
#' bad argument error handling function
#'
#' @param  arg  argument choosen
#'
#' @keywords internal

	stop("Unrecognized option ",arg," for argument 'src', valid options are: 'livedata','repo','local'.")
}


#############################################################################
#############################################################################


covid19.genomic.data <- function(type='genome', src="livedata", graphics.ON=TRUE, accOnly=TRUE) {
#' main master (wrapper) function to obtain different types of genomic data for the SARS-CoV-2 virus
#'
#' @param  type  type of data to retrieve, options are: 'genome', 'genomic', 'fasta', 'nucleotide', 'protein', 'ptree'
#' @param  src  source of the data: "livedata", "repo" or "local"
#' @param  graphics.ON  boolean option for display associated graphics
#' @param  accOnly  boolean indicator for getting only accession codes or whole records
#'
#' @export
#'

	if (tolower(type)=='genome') {
		# >>> WORKING!!!
		# get the genome data
		c19.g.data <- c19.refGenome.data(src=src, graphics.ON=graphics.ON)
		return(c19.g.data)
	} else if (tolower(type)=='genomic') {
		#### >>>>> CHECK <<<<<<<<<<<
		# get the composed data
		c19.gs.data <- c19.genomic.data(src=src)
		return(c19.gs.data)
	} else if (tolower(type)=='fasta') {
		# >>> WORKING!!!
		# get the fasta data
		c19.fasta <- c19.fasta.data(src)
		return(c19.fasta)
	#########
	} else if (tolower(type)=='nucleotide') {
		#### >>>>> **NOT** WORKING <<<<<<<<<<<
		# get nucleotides data
		c19.nucs.data <- c19.NPs.data(src=src,DB='nucleotide', accOnly=accOnly)
		return(c19.nucs.data)
	} else if (tolower(type)=='protein') {
		#### >>>>> **NOT** WORKING <<<<<<<<<<<
		# get proteins data
		c19.prots.data <- c19.NPs.data(src=src,DB='protein', accOnly=accOnly)
		return(c19.prots.data)
	#########
	} else if (tolower(type)=='nucleotide-fasta') {
		c19.fasta.data <- c19.NP_fasta.data(src=src,target='nucleotide')
		return(c19.fasta.data)
	} else if (tolower(type)=='protein-fasta') {
		c19.fasta.data <- c19.NP_fasta.data(src=src,target='protein')
		return(c19.fasta.data)
	} else if (tolower(type)=='codingregion-fasta') {
		c19.fasta.data <- c19.NP_fasta.data(src=src,target='codingRegion')
		return(c19.fasta.data)
	#########
	} else if (tolower(type)=='ptree') {
		# >>> WORKING!!!
		# get the phylogenetic tree
		c19.ptree <- c19.ptree.data(src=src)
		return(c19.ptree)
	#########
	} else {
		warning("Unrecongized option! Valid options are: 'genome', 'genomic', 'fasta', 'nucleotide', 'protein', 'ptree'")
	}
	#########
}


##################


red.devel.ver <- function(fileRDS, force=TRUE) {
#' function to redirect users to install devel version from github
#' @param  fileRDS  missing data file
#' @param  force  boolean flag to force stoping the code
#'

	message("Missing file", fileRDS)

	message("Please install the developemnt version of the package to access the local datasets!")
	message('You will need the "devtools" package -- install it using \t install.packages("devtools")')
	message('Then the develoment version of the covid19.analytics package can be installed using the following command',
		'\n', '\t  devtools::install_github("mponce0/covid19.analytics")')

	if (force) stop("Error: ",fileRDS," not found!")
}


##################


c19.refGenome.data <- function(src='livedata', graphics.ON=TRUE) {
#' function to obtain sequencing data grom NCBI
#' Reference:  https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2
#'
#' @param  src  data origin source: 'livedata', 'repo', 'local'
#' @param  graphics.ON  flag to activate/deactivate graphical output
#'
#' @importFrom  ape  read.GenBank
#
#' @export
#'
#' @examples
#' # obtain covid19's genomic data
#' covid19.gen.seq <- c19.refGenome.data()
#' # display the actual RNA seq
#' covid19.gen.seq$NC_045512.2
#'


        ###############################

        ## function for error handling
        errorHandling.Msg <- function(condition,target.case) {
                #header('=')
                message("A problem was detected when trying to retrieve the data using: ",target.case)
                if (grepl("404 Not Found",condition)) {
                        message("The URL or file was not found! Please contact the developer about this!")
                } else {
                        message("It is possible that your internet connection is down! Please check!")
                }
                message(condition,'\n')
                #header('=')

                # update problems counter
                #pkg.env$problems <- pkg.env$problems + 1

        }

        ###############################

	covid19.pckg <- 'covid19.analytics'
	target <- "NC_045512.2"

	if (src=='livedata') {
		# load 'ape' library
		loadLibrary("ape")

		# retrieve genetic data
		message("Retrieving data from NCBI...")
		tryCatch({
				covid19.seq <- read.GenBank(target,as.character=TRUE)
			},
			# warning
			warning = function(cond) {
					errorHandling.Msg(cond,src)
			},
			# error
			error = function(e){
					errorHandling.Msg(e,src)
			}
		)

	} else if (src=='repo') {
		message("Retrieving data from backup-repo...")
		fileRDSname <- paste0(target,".rds")
		fileRDS <- normalizePath(file.path(tempdir(), fileRDSname), mustWork=FALSE)
		tryCatch({
				download.file("https://github.com/mponce0/covid19analytics.datasets/raw/master/genomics.data/NC_045512.2.rds",fileRDS)
				c19data <- load(fileRDS)
			},
                        # warning
                        warning = function(cond) {
				errorHandling.Msg(cond,src)
                        },
                        # error
                        error = function(e){
				errorHandling.Msg(e,src)
                        }
                )
	} else if (src=='local') {
		message("Retrieving data from local-data...")
		fileRDS <- paste0(target,".rds")
		fileRDS <- system.file("extdata",fileRDS, package=covid19.pckg, mustWork = TRUE)
		if (file.exists(fileRDS)) {
			c19data <- load(fileRDS)
		} else {
			red.devel.ver(fileRDS, force=TRUE)
			#stop("Error: ",fileRDS," not found!")
		}
	} else {
		badOption(src)
	}


	if ("covid19.seq" %in% ls()) {
	message(summary(covid19.seq))

	if (graphics.ON) {
		freq.table.ACGT <- table(covid19.seq$NC_045512.2)
		bplt <- barplot(freq.table.ACGT/sum(freq.table.ACGT),
				col=heat.colors(4),
				main="ACTG Distribution in covid19 genome")
		#text(names(freq.table.ACGT),bplt+3,freq.table.ACGT,xpd=TRUE, col='blue')
	}

	return(covid19.seq)
	}

	##
	if (src=='livedata') {
		e_c19 <-  c19.refGenome.data(src='repo')
	}
	#
	if (src=='repo') {
		e_c19 <- c19.refGenome.data(src='local')
	}
	#
	if ("e_c19" %in% ls()) return(e_c19)
	##
}


######


c19.fasta.data <- function(src='livedata') {
#' function to obtain FASTA sequence of the SARS-CoV-2 virus
#'
#' @param  src  argument to indicate where the data is being retrieved from
#'
#' @export
#'
#' @importFrom  ape  read.FASTA
#'

        ###############################

        ## function for error handling
        errorHandling.Msg <- function(condition,target.case) {
                #header('=')
                message("A problem was detected when trying to retrieve the data using: ",target.case)
                if (grepl("404 Not Found",condition)) {
                        message("The URL or file was not found! Please contact the developer about this!")
                } else {
                        message("It is possible that your internet connection is down! Please check!")
                }
                message(condition,'\n')
                #header('=')
                
                # update problems counter
                #pkg.env$problems <- pkg.env$problems + 1
                
        }

        ###############################

	covid19.pckg <- 'covid19.analytics'

	if ( src=='livedata' | src=='repo' ) {
		fastaURL <- "https://raw.githubusercontent.com/mponce0/covid19analytics.datasets/master/genomics.data/"
                fasta.file.name <- "c19.fasta"
		cv19FASTAloc <- getFile(fastaURL,fasta.file.name)	
	} else if (src=='local') {
		cv19FASTAloc <- 'c19.fasta'
		cv19FASTAloc <- system.file("extdata",cv19FASTAloc, package=covid19.pckg, mustWork = TRUE)
	} else {
		badOption(src)
	}

	loadLibrary("ape")	
	### Retrieve FASTA . . .
	# FASTA  -- is obtained locally as it is not expectd to change...
	tryCatch({
		cv19_fasta <- read.FASTA(file=cv19FASTAloc)
		},
		# warning
		warning = function(cond) {
				errorHandling.Msg(cond,src)
		},
		# error
		error = function(e){
				errorHandling.Msg(e,src)
		}
	)

	if ("cv19_fasta" %in% ls()) return(cv19_fasta)

        ##
        if (src=='livedata') {
                e_c19_fasta <-  c19.fasta.data(src='repo')
        }
        #
        if (src=='repo') {
                e_c19_fasta <- c19.fasta.data(src='local')
        }
        #
        if ("e_c19_fasta" %in% ls()) return(e_c19_fasta)
        ##
}


######


c19.ptree.data <- function(src='livedata') {
#' function to obtain "Tree of complete SARS-CoV-2 Sequences as obtained from NCBI"
#'
#' @param  src  argument to indicate where the data is being retrieved from
#'
#' @export
#'
#' @importFrom  ape  read.tree
#'
        ###############################

        ## function for error handling
        errorHandling.Msg <- function(condition,target.case) {
                #header('=')
                message("A problem was detected when trying to retrieve the data using: ",target.case)
                if (grepl("404 Not Found",condition)) {
                        message("The URL or file was not found! Please contact the developer about this!")
                } else {
                        message("It is possible that your internet connection is down! Please check!")
                }
                message(condition,'\n')
                #header('=')

                # update problems counter
                #pkg.env$problems <- pkg.env$problems + 1

        }

        ###############################

	covid19.pckg <- 'covid19.analytics'

        if ( src=='livedata' ) {
		tryCatch({
			#treeURL <- "https://www.ncbi.nlm.nih.gov/projects/treeview/ncfetch.cgi?key=NCID_1_6668093_130.14.22.93_9105_1596057400_2120854399_0MetA0___S_NC_TreeView_PROD_F_1&fmt=text/plain&filename="
			#	"https://www.ncbi.nlm.nih.gov/projects/treeview/ncfetch.cgi?key=NCID_1_7796085_130.14.22.93_9105_1596148318_79646198_0MetA0___S_NC_TreeView_PROD_F_1&fmt=text/plain&filename=tree.nwk"
			#	"https://www.ncbi.nlm.nih.gov/projects/treeview/ncfetch.cgi?key=NCID_1_9994809_130.14.18.4_9105_1596398515_81911838_0MetA0___S_NC_TreeView_PROD_F_1&fmt=text/plain&filename=tree.nwk"
			#tree.file.name <- "tree.nwk"
			treeURL <- "https://github.com/mponce0/covid19analytics.datasets/raw/master/genomics.data/"
			tree.file.name <- "cv19tree.nwk"
			cv19treeloc <- getFile(treeURL,tree.file.name)
			if (file.exists(cv19treeloc)) {
				cv19tree <- read.tree(cv19treeloc)
				return(cv19tree)
				#treename <- load(cv19treeloc)
				#return(eval(parse(text = treename)))
			} else {
				stop("Problem downloading tree file from NCBI")
			}
			},
			# warning
			warning = function(cond) {
					errorHandling.Msg(cond,src)
			},
			# error
			error = function(e){
					errorHandling.Msg(e,src)
			}
		)
	} else if (src=='repo') {
                tryCatch({
                        treeURL <- "https://github.com/mponce0/covid19analytics.datasets/raw/master/genomics.data/"
                        tree.file.name <- "cv19tree.rds"
                        cv19treeloc <- getFile(treeURL,tree.file.name)
                        },
                        # warning
                        warning = function(cond) {
                                        errorHandling.Msg(cond,src)
                        },
                        # error
                        error = function(e){
                                        errorHandling.Msg(e,src)
                        }
		)
        } else if (src=='local') {
                cv19treeloc <- 'cv19tree.rds'
		cv19treeloc <- system.file("extdata",cv19treeloc, package=covid19.pckg, mustWork = TRUE)
        } else {
                badOption(src)
        }

        if (file.exists(cv19treeloc)) {
		load(cv19treeloc)

		if ("cv19tree" %in% ls()) return(cv19tree)
	} else {
		red.devel.ver(cv19treeloc, force=TRUE)
	}

        ##
        if (src=='livedata') {
                e_cv19tree <-  c19.ptree.data(src='repo')
        }
        #
        if (src=='repo') {
                e_cv19tree <- c19.ptree.data(src='local')
        }
        #
        if ("e_cv19tree" %in% ls()) return(e_cv19tree)
        ##
}


######

c19.genomic.data <- function(src='livedata', accOnly=TRUE) {
#' function to obtain genomic data from SARS-CoV-2019
#'
#' @param  src  argument to indicate what sources are going to be used for retrieving the data: "livedata", "repo" or "local"
#'		'livedata' will access NCBI servers to acquire the latest possible data, this may incur in significant longer times
#'	      'repo' will access an updated replica of the data from a github repository (faster but not necessarily upto the latest udpates)
#'	      'local' will access previously archived records within teh package (fastest but not updated)
#' @param  accOnly  boolean indicator for getting only accession codes or whole records
#'
#' @return a list containing reference genome, annotation data, nucleotides, proteins and list of SRA runs
#'
#' @export
#'
#' @importFrom  ape  read.gff
#'

	### Set URLs...
	if (src=='livedata') {
		# URLS
		# Complete genome
		descrp <- "NCBI DataBases"
		completeGenome_URL <- "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512"
		nucleotides_URL <- "https://www.ncbi.nlm.nih.gov/sars-cov-2/download-nuccore-ids/"
		annotation_URL <- "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/009/858/895/GCF_009858895.2_ASM985889v3/"
		sra_URL <- "https://ftp-trace.ncbi.nlm.nih.gov/sra/reports/AccList/"
		URLs <- list(descrp, completeGenome_URL,nucleotides_URL,annotation_URL,sra_URL)
	} else if (src=='repo') {
		descrp <- "BCKUP GIThub repo"
		#URL <- "https://github.com/mponce0/covid19analytics.datasets/raw/master/genomic.data/"
		URL <- "https://raw.githubusercontent.com/mponce0/covid19analytics.datasets/master/genomics.data/"
		annotation_URL <- sra_URL <- URL
		URLs <- list(descrp, URL)
	} else if (src=='local') {
		descrp <- "covid19.analytics -- local data"
		URL <- paste0(system.file("extdata",package='covid19.analytics',mustWork=TRUE),'/')
		annotation_URL <- sra_URL <- URL
		URLs <- list(descrp, URL)
	} else {
		badOption(src)
	}


	### Retrieve GENOME . . .
	# genomic data
	# reference genome from GenBank
	gen_data <- c19.refGenome.data(src=src,graphics.ON=FALSE)


	### Retrieve FASTA . . .
	# FASTA  -- is obtained locally as it is not expectd to change...
	#cv19_fasta <- read.FASTA(file="c19.fasta")
	cv19_fasta <- c19.fasta.data(src)

	######
	### Sequence List
	## list of SARS-CoV-2 nucleotide sequences
	## https://www.ncbi.nlm.nih.gov/sars-cov-2/download-nuccore-ids/
	######
	# this obtains the latest 10000 nucletoides
#	nuc.ids <- read.csv(nucleotides_URL)
	# which should be complemented with the total list of nucleotides available at,
	#	https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Nucleotide&VirusLineage_ss=SARS-CoV-2,%20taxid:2697049
	# and stored in an RDS object --> nucls as a "basal" data set
#	load("sequences-nucleotides-10951-2020jul19.rds")
#	all_nucls_ids <- unique(nucls$Accession, nuc.ids)


	### ANNOTATION . . .
	#######
	loadLibrary("ape")
	if (src=='livedata' | src=='repo') {
		## NCBI RefSeq for SARS-CoV-2 genome (NC_045512)
		# ANNOTATION
		#ann.url <- "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/009/858/895/GCF_009858895.2_ASM985889v3/"
		ann.url <- annotation_URL
		ann.file.name <- "GCF_009858895.2_ASM985889v3_genomic.gff.gz"
                ann.file <- normalizePath(file.path(tempdir(), ann.file.name), mustWork=FALSE)
		ann.remote.loc <- paste0(ann.url,ann.file.name)
		download.file(ann.remote.loc,ann.file)
		#ann.data <- read.gff(ann.file)
	} else if (src=='local') {
		ann.file <- paste0(URL,"GCF_009858895.2_ASM985889v3_genomic.gff.gz")
		#ann.data <- read.gff(ann.file)
	} else {
		badOption(src)
	}
	if (file.exists(ann.file)) {
		ann.data <- read.gff(ann.file)
	} else {
		warning("File not found: ",ann.file)
		red.devel.ver(ann.file, force=FALSE)
	}


	### SRAs . . .
	#######
	if (src=='livedata' | src=='repo' | src=='local') {
		## SARS-CoV-2 and Coronaviridae family next-generation sequencing runs in SRA
		## upper dir
		## https://ftp-trace.ncbi.nlm.nih.gov/sra/reports/AccList/
		# README
		# https://ftp-trace.ncbi.nlm.nih.gov/sra/reports/AccList/README.txt
		# runs
		# https://ftp-trace.ncbi.nlm.nih.gov/sra/reports/AccList/Coronaviridae_runs.csv
		########
		sra_info <- readLines(paste0(sra_URL,"/","README.txt"))
		if (src !=  'local') {
			sra_runs <- read.csv(paste0(sra_URL,"/","Coronaviridae_runs.csv"))
		} else {
			load(paste0(sra_URL,"/","Coronaviridae_runs.csv",'.rds'))
		}
	} else {
		badOption(src)
	}
		

	###  NUCLEOTIDES  &  PROTEINS
	nucleotides <- c19.NPs.data(src=src,DB='nucleotide', accOnly=accOnly)
	proteins <- c19.NPs.data(src=src,DB='protein', accOnly=accOnly)


	return(list(	genome=gen_data,		# working
			fasta=cv19_fasta,
			annotation=ann.data,		# working
			nucleotides = nucleotides,	#lst.nucs,		# local or live
			proteins = proteins,
			SRA=list(sra_info=sra_info,sra_runs=sra_runs),	# working
			references=URLs
			)
		)
}


##########################


        ###  NUCLEOTIDES  &  PROTEINS
#       for ( i in nuc.ids[[1]] ){
#               print(i)
#               lst.nucs <- c(lst.nucs, read.GenBank(i, as.character=TRUE))
#       }
#       lst.nucs <- nuc.ids[[1]]

c19.NPs.data <- function(src='livedata', DB='nucleotide', max.nr.recs=NULL, accOnly=TRUE ) {
#' function to obtain data for nucleotides or proteins from SARS-CoV-2
#'
#' @param  src  origin for the data source: "livedata", "repo", "local"
#' @param  DB  database
#' @param  max.nr.recs  maximun number of records to retrieve, there are limitations in the fns and server sides
#' @param  accOnly  boolean indicator for getting only accession codes or whole records
#'
#' @export
#'
	if (DB=='nucleotide') {
		file.tgt <- "sequences-nucleotides.rds"
		if (is.null(max.nr.recs)) max.nr.recs <- 50000
	} else if (DB=='protein') {
		file.tgt <- "sequences-proteins.rds"
		if (is.null(max.nr.recs)) max.nr.recs <- 150000
	} else {
		stop("Only 'nucleotide' or 'protein' are valid DB options")
	}

        if (src=='livedata') {
                message("Retrieving ",DB," data from NCBI servers, this will retrieve the latest dataset but may take longer times...")
#                lst.nucs <- avecRentrez(DB='nucleotide',max.nr.recs=50000)
#                proteins <- avecRentrez(DB='protein',max.nr.recs=150000)
		target <- avecRentrez(DB=DB, max.nr.recs=max.nr.recs, accOnly=accOnly)
		return(target)
        } else if (src=='repo') {
		URL <- "https://raw.githubusercontent.com/mponce0/covid19analytics.datasets/master/genomics.data/"
                message("Retriving ",DB," data from *backup repo*, this may not include the latest updated data")
                symLink <- readLines(paste0(URL,file.tgt), warn=FALSE)
                nfile <- getFile(URL,symLink)
                if (file.exists(nfile)) {
			load(nfile)
		} else {
			stop(nfile, "NOT Found!")
		}
                #lst.nucs <- nucleotides
#                symLink <- readLines(paste0(URL,"sequences-proteins.rds"), warn=FALSE)
#                pfile <- getFile(URL,symLink)
#                load(pfile)
        } else if (src=='local') {
                message("Retriving ",DB," data stored *locally*, this may not include the latest updated data")
######### OLD WAY, not used... ####
if (FALSE) {
nucleotides_URL <- "https://www.ncbi.nlm.nih.gov/sars-cov-2/download-nuccore-ids/"
                ######
                # this obtains the latest 10000 nucletoides
                nuc.ids <- read.csv(nucleotides_URL)
                # which should be complemented with the total list of nucleotides available at,
                #       https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Nucleotide&VirusLineage_ss=SARS-CoV-2,%20taxid:2697049
                # and stored in an RDS object --> nucls as a "basal" data set
                # --> nucleotides
                load("sequences-nucleotides-10951-2020jul19.rds")
                # working by combining a "basal" dataset + latest 10K
                all_nucls_ids <- unique(nucls$Accession, nuc.ids)

                message(paste("Basal nbr of nucleotides: ",length(nucls$Accession)) )
                message(paste("Number of nucleotides: ",length(all_nucls_ids)) )

                lst.nucs <- all_nucls_ids
}
####################################
                tgt.file <- system.file("extdata",file.tgt, package='covid19.analytics',mustWork=TRUE)
                if (file.exists(tgt.file)) {
			nucleotides <- proteins <- NULL
                        load(tgt.file)        # laod  'nucleotides' --or-- 'proteins'
                } else {
			red.devel.ver(file.tgt, force=FALSE)
                        stop(DB," file: ", file.tgt," -- ",tgt.file," -- missing!")
                }
                #lst.nucs <- nucleotides

                # Retrieve proteins' sequences --> proteins
                #prots.file <- system.file("extdata","sequences-proteins.rds", package='covid19.analytics',mustWork=TRUE)
                #if (file.exists(prots.file)) {
                #        load(prots.file)        # load 'proteins'
                #} else {
                #        stop("Nucleotides file: 'sequences-proteins.rds' -- ", prots.file," -- missing!")
                #}
        } else {
                badOption(src)
	}

	if (DB=='nucleotide') {
		if ('nucleotides'%in%ls() & !is.null(nucleotides)) {
			return(nucleotides)
		} else {
			stop("Error: something went wrong reading the nucleotides data")
		}
	} else if (DB=='protein') {
		if ('proteins' %in% ls() & !is.null(proteins)) {
			return(proteins)
		} else {
			stop("Error: something went wrong reading the proteins data")
		}
	} else {
		warning("Unrecognized option")
	}
}


############

c19.NP_fasta.data <- function(src='repo', target='nucleotide' ) {
#' function to obtain FASTA seqs for nucleotides or proteins from SARS-CoV-2
#'
#' @param  src  origin for the data source: "livedata" OR "repo"
#' @param  target  "nucleotide", "protein" or "codingRegion"
#'
#' @export
#'
        if (target=='nucleotide') {
                file.tgt <- "sequences-nucleotides-fasta.rds"
        } else if (target=='protein') {
                file.tgt <- "sequences-proteins-fasta.rds"
        } else if (target=='codingRegion') {
		stop("codingRegion not currently avaialble...")
	} else {
                stop("Only 'nucleotide', 'protein' or 'codingRegion' are valid target options")
        }

        if (src=='livedata') {
                #message("Retrieving ",target," data from NCBI servers, this will retrieve the latest dataset but may take longer times...")
                #target <- avecRentrez(DB=target, max.nr.recs=max.nr.recs)
                #return(target)
		message("not available, please use 'repo' isntead")
        } else if (src=='repo') {
                URL <- "https://raw.githubusercontent.com/mponce0/covid19analytics.datasets/master/genomics.data/"
                message("Retriving ",target," data from *backup repo*, this may not include the latest updated data")
                symLink <- readLines(paste0(URL,file.tgt), warn=FALSE)
                nfile <- getFile(URL,symLink)
                if (file.exists(nfile)) {
			#loadLibrary("ape")
                        #target <- read.FASTA(nfile)
			target <- load(nfile)
			return(eval(parse(text = target)))
                } else {
                        stop(nfile, "NOT Found!")
                }
        } else if (src=='local') {
                message("FASTA files are too large to be stored with the package locally, please use instead the 'repo' src")
        } else {
                badOption(src)
        }

}

############


avecRentrez <- function(DB="nucleotide", max.nr.recs=20000, chunkSize=100, accOnly=TRUE) {
#' function to query NCBI database servers using the "rentrex" library
#'
#' @param  DB  database
#' @param  max.nr.recs  maximun number of records to retrieve, there are limitations in the fns and server sides
#' @param  chunkSize  number of records to retrieve at once
#' @param  accOnly  boolean indicator for getting only accession codes or whole records
#'
#' @keywords internal
#'
#' @importFrom  rentrez  entrez_search  entrez_summary  extract_from_esummary
#' @importFrom  utils  setTxtProgressBar  txtProgressBar
#'

	# load library 'rentrez'
	loadLibrary("rentrez")

	# keyword to search for ...
	SARScov2 <- "Severe acute respiratory syndrome coronavirus 2"

	# search for covid19 SARS-COV-2 in nucleotides
	res <- entrez_search(db=DB, term=SARScov2,
				retmax=max.nr.recs    #, use_history=TRUE
			)

	# check
	if (length(res$ids) < res$count)
		warning(length(res$ids), "vs ", res$count)

	message("Retrieving data from NCBI ",DB," databases...")

	# progress bar
	tots <- res$count
	pbar <- txtProgressBar(min = 0, max = tots, style = 3)

	# get list of nucleotides
	nuc.ids <- c()
	for (j in seq(1,res$count,chunkSize) ) {
		#print(j)
		setTxtProgressBar(pbar,j)
		#cat(paste(round((j/res$count)*100,0),"%"))
		rec.range <- j:min(j+chunkSize,res$count)
		#print(rec.range)

		recs <- entrez_summary(db='nucleotide', id=res$ids[rec.range])
		if (accOnly) {
			nuc.ids <- c(nuc.ids, extract_from_esummary(recs,"caption"))
		} else {
			nuc.ids <- c(nuc.ids, recs)
		}
	}
	close(pbar)

	return(nuc.ids)
}


##############################################################################
