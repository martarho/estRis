#' RetrieveProjectData
#'
#' This function retrieves all associated metadata and urls to an SRP id

RetrieveProjectMetadata = function(PID) {
	ena.url <- paste("http://www.ebi.ac.uk/ena/data/warehouse/filereport?accession=", PID,"&result=read_run",sep="")
	metadata <- data.frame(read.table(url(ena.url), header=TRUE, sep="\t"), stringsAsFactors=F)	
	return(metadata)	
}

#' RetrieveProjectData
#'
#'Downloads FASTQ files using a metadata data.frame or a list

DownloadFASTQ = function(l, folder="./") {
	if ("data.frame" %in% is(l)) { l = l$fastq_ftp }
	if (TRUE %in% (grepl(";",l))) { l = unlist( strsplit(as.character(l), ';')) }
	lapply(l, function(x) {
		fname = basename(as.character(x))
		cat(paste("#Starting",fname,"\n",sep=" "))
		download.file(
			paste("http://", as.character(x), sep=""), 
			destfile=paste(folder, fname, sep=""),
			quiet=T
			)
		cat(paste("#Finished",fname,"\n",sep=" "))
		})
}

#' ParallelDownloadFASTQ
#'
#' Parallel Downloads FASTQ using doMC and a metadata data.frame

ParallelDownloadFASTQ = function(df, folder="./", n=6) {
	library(plyr)
	library(doMC)
	doMC::registerDoMC(cores=n)
	llply(split(df, df$run_accession), DownloadFASTQ, folder=folder, .parallel=T)
}
