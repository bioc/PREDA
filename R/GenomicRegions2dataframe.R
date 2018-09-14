
##
## function GenomicRegions2dataframe to extract regions as a dataframe
##

##
## GenomicRegions - GenomicRegions2dataframe
##

GenomicRegions2dataframe<-function(GenomicRegionsObject) {

  if (is.null(GenomicRegionsObject)) {
  warning("input GenomicRegionsObject is null in GenomicRegions2dataframe. Returning NULL value")
  return(NULL)
  } else if (inherits(GenomicRegionsObject, what="GenomicRegions")) {
    output_dataframe<-data.frame("chr"=slot(GenomicRegionsObject, "chr"), "start"=slot(GenomicRegionsObject, "start"), "end"=slot(GenomicRegionsObject, "end"), stringsAsFactors=FALSE)
        if (!is.null(slot(GenomicRegionsObject, "ids"))) {
        output_dataframe<-data.frame("ids"=slot(GenomicRegionsObject, "ids"), output_dataframe, stringsAsFactors=FALSE)
        rownames(output_dataframe)<-output_dataframe[,"ids"]
        }
        if (!is.null(slot(GenomicRegionsObject, "optionalAnnotations"))) {
        optionalAnnotations<-slot(GenomicRegionsObject, "optionalAnnotations")
        optionalAnnotationsHeaders<-slot(GenomicRegionsObject, "optionalAnnotationsHeaders")
        colnames(optionalAnnotations)<-optionalAnnotationsHeaders
        output_dataframe<-cbind(output_dataframe, optionalAnnotations, stringsAsFactors=FALSE)
        }

    return(output_dataframe)
  } else {
  stop("in GenomicRegions2dataframe the input GenomicRegionsObject must be either a GenomicRegions or NULL")
  }

}




