##
## GenomicAnnotationsForPREDA - GenomicAnnotationsForPREDA2dataframe
##

setMethod("GenomicAnnotationsForPREDA2dataframe", "GenomicAnnotationsForPREDA", function(.Object) {
output_dataframe<-GenomicAnnotations2dataframe(.Object)
return(output_dataframe)
})

