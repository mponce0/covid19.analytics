library(collapsibleTree)

# retrieve the nucleotides data
nucx <- covid19.genomic.data(type='nucleotide',src='repo')

# identify specific fields to look at
len.fld <- "Length"
acc.fld <- "Accession"
geoLoc.fld <- "Geo_Location"
seq.fld <- "Sequence_Type"
host.fld <- "Host"

seq.limit1 <- 25000

seq.limit1 <- 29600
seq.limit2 <- 31000

# selection criteria, nucleotides with seq.length between 29600 and 31000
selec.ctr.1 <- nucx$Length<seq.limit2 & nucx$Length>seq.limit1

# remove nucletoides without specifying a "host"
targets <- nucx[selec.ctr.1 & nucx$Host!='',]

# categorization tree based on Host-type, geographical location, ...
collapsibleTreeSummary(targets,
                hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
                attribute=len.fld,
                zoomable=FALSE, collapsed=TRUE)

#  categorization tree for nucleotides with seq. lengths smaller than 29600 or bigger than 31000
collapsibleTreeSummary(nucx[!selec.ctr.1,],
                hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
                attribute=len.fld,
                zoomable=FALSE, collapsed=TRUE)

#  categorization tree for "human hosted" nucleotides with seq. lengths smaller than 29600 or bigger than 31000
collapsibleTreeSummary(nucx[!selec.ctr.1 &  nucx$Host=='Homo sapiens',],
                hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
                attribute=len.fld,
                zoomable=FALSE, collapsed=TRUE)
