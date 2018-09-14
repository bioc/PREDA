
##
## method PREDADataAndResults2dataframe to extract annotations as a dataframe with probeids as rownames
##

##
## PREDADataAndResults - PREDADataAndResults2dataframe
##

setMethod("PREDADataAndResults2dataframe", "PREDADataAndResults", function(.Object) {

output_dataframe<-PREDAResults2dataframe(.Object)
statistic<-slot(.Object, "statistic")

output_dataframe<-data.frame(output_dataframe, statistic, stringsAsFactors=FALSE)
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)
})

