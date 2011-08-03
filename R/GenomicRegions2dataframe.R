
##
## method GenomicRegions2dataframe to extract regions as a dataframe
##

##
## GenomicRegions - GenomicRegions2dataframe
##

setMethod("GenomicRegions2dataframe", "GenomicRegions", function(.Object) {
output_dataframe<-data.frame("chr"=slot(.Object, "chr"), "start"=slot(.Object, "start"), "end"=slot(.Object, "end"), stringsAsFactors=FALSE)
    if (!is.null(slot(.Object, "ids"))) {
    output_dataframe<-data.frame("ids"=slot(.Object, "ids"), output_dataframe, stringsAsFactors=FALSE)
    rownames(output_dataframe)<-output_dataframe[,"ids"]
    }
    if (!is.null(slot(.Object, "optionalAnnotations"))) {
    optionalAnnotations<-slot(.Object, "optionalAnnotations")
    optionalAnnotationsHeaders<-slot(.Object, "optionalAnnotationsHeaders")
    colnames(optionalAnnotations)<-optionalAnnotationsHeaders
    output_dataframe<-cbind(output_dataframe, optionalAnnotations, stringsAsFactors=FALSE)
    }

return(output_dataframe)
})

