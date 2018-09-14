##
## GenomicAnnotations - GenomicAnnotations2dataframe
##

setMethod("GenomicAnnotations2dataframe", "GenomicAnnotations", function(.Object) {
output_dataframe<-data.frame("ids"=slot(.Object, "ids"), "chr"=slot(.Object, "chr"), "start"=slot(.Object, "start"), "end"=slot(.Object, "end"), "strand"=slot(.Object, "strand"), stringsAsFactors=FALSE)

    if (!is.null(slot(.Object, "optionalAnnotations"))) {
    optionalAnnotations<-slot(.Object, "optionalAnnotations")
    optionalAnnotationsHeaders<-slot(.Object, "optionalAnnotationsHeaders")
    colnames(optionalAnnotations)<-optionalAnnotationsHeaders
    output_dataframe<-cbind(output_dataframe, optionalAnnotations, stringsAsFactors=FALSE)
    }
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)
})


##
## GenomicAnnotationsForPREDA - GenomicAnnotations2dataframe
##

setMethod("GenomicAnnotations2dataframe", "GenomicAnnotationsForPREDA", function(.Object) {

GenomicAnnotations_object<-GenomicAnnotationsForPREDA2GenomicAnnotations(.Object)
output_dataframe<-GenomicAnnotations2dataframe(GenomicAnnotations_object)
output_dataframe<-data.frame(output_dataframe, "position"=slot(.Object, "position"), stringsAsFactors=FALSE)
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)

})

