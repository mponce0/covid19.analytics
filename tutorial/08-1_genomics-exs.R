# test all sources...
srcs <- c('livedata','repo','local')

results <- list()

for (sr in srcs) {
	print(paste("##### ",sr," #####"))

	# obtain reference genome data
	results[['refGenome']][sr] <- c19.refGenome.data(src=sr)

	# obtain phylogenetic tree
	results[['tree']][sr] <- c19.ptree.data(src=sr)

	# obtain FASTA sequences
	results[['fasta']][sr] <- c19.fasta.data(src=sr)

	# combine several types
	if (sr != 'livedata') {
		results[['genomics']][sr] <- c19.genomic.data(src=sr)
	}

	# obtain nucleotides data
	results[['nucs']][sr] <- c19.NPs.data(src=sr, DB='nucleotide')
	if (sr != 'livedata') {
		results[['ptns']][sr] <- c19.NPs.data(src=sr, DB='protein')
	}

	# obtain FASTA sequences for nucleotides and proteins
	results[['FASTAs']][['nucs']][sr] <- c19.NP_fasta.data(target='nucleotide')
	results[['FASTAs']][['ptns']][sr] <- c19.NP_fasta.data(target='protein')
}



#####

gtypes <- c("genome","fasta","tree",
		"nucleotide","protein",
		"nucleotide-fasta","protein-fasta",
		"genomic")

results2 <- list()

for (gt in gtypes) {
	print(gt)

	results2[[gt]] <- covid19.genomic.data(type=gt)
}
