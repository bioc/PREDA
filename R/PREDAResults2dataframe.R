
##
## method PREDAResults2dataframe: extract annotations and PREDA output statistics as a dataframe with probeids as rownames
##

##
## PREDAResults - PREDAResults2dataframe
##

setMethod("PREDAResults2dataframe", "PREDAResults", function(.Object) {

output_dataframe<-GenomicAnnotations2dataframe(.Object)

smoothStatistic<-slot(.Object, "smoothStatistic")
pvalue<-slot(.Object, "pvalue")
qvalue<-slot(.Object, "qvalue")
analysesNames<-slot(.Object, "analysesNames")
colnames(smoothStatistic)<-paste("smoothStatistic", analysesNames, sep=".")
colnames(pvalue)<-paste("pvalue", analysesNames, sep=".")
colnames(qvalue)<-paste("qvalue", analysesNames, sep=".")

output_dataframe<-data.frame(output_dataframe, smoothStatistic, pvalue, qvalue, stringsAsFactors=FALSE)
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)
})

