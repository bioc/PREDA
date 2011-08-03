
##
## method GenomicRegionsAnnotate 
##

##
## GenomicRegions & GenomicAnnotations - GenomicRegionsAnnotate
##

setMethod("GenomicRegionsAnnotate", signature=c("GenomicRegions", "GenomicAnnotations"), function(.Object1, .Object2, AnnotationsHeaders=NULL, sep.character="; ", complete.inclusion=FALSE, annotationAsRange=FALSE, getJustFeaturesNumber=FALSE) {

## a parameter to avoid GenomicRegionsSortAndCleanNA within GenomicAnnotationsExtract and one unique call to GenomicRegionsSortAndCleanNA has been added

  if (is.null(AnnotationsHeaders)) {
    if (!(getJustFeaturesNumber)) {
        if (is.null(slot(.Object2, "optionalAnnotationsHeaders"))) {
        return(.Object1)
        } else {
        AnnotationsHeaders<-slot(.Object2, "optionalAnnotationsHeaders")
        }
    } else {
    AnnotationsHeaders<-"Features Number"
    }
  } else {
    if (!is.character(AnnotationsHeaders)) {
    stop("Please specify selected annotations as a character vector")
    }
  }

    if (getJustFeaturesNumber) {
    annotation_matrix<-matrix(nrow=GenomicRegionsNumber(.Object1), ncol=1)
    } else {
    annotation_matrix<-matrix(nrow=GenomicRegionsNumber(.Object1), ncol=length(AnnotationsHeaders))
    }

regions_matrix<-GenomicRegions2dataframe(.Object1)


.Object2<-GenomicAnnotationsSortAndCleanNA(.Object2)


    for (i in 1:nrow(annotation_matrix)) {
        for (j in 1:ncol(annotation_matrix)) {
            if (getJustFeaturesNumber) {
            AnnotationsHeaderToextract<-NULL
            } else {
            AnnotationsHeaderToextract<-AnnotationsHeaders[j]
            }
        annotation_matrix[i,j]<-GenomicAnnotationsExtract(.Object2, chr=regions_matrix[i,"chr"], start=regions_matrix[i,"start"], end=regions_matrix[i,"end"], AnnotationsHeader=AnnotationsHeaderToextract, sep.character=sep.character, complete.inclusion=complete.inclusion, skipSorting=TRUE, annotationAsRange=annotationAsRange, getJustFeaturesNumber=getJustFeaturesNumber)
        }
    }


GenomicRegions_data_object<-new("GenomicRegions",
 ids=slot(.Object1, "ids"),
 chr=slot(.Object1, "chr"),
 start=slot(.Object1, "start"),
 end=slot(.Object1, "end"),
 chromosomesNumbers=slot(.Object1, "ids"),
 chromosomesLabels=slot(.Object1, "ids"),
 optionalAnnotations=cbind(slot(.Object1, "optionalAnnotations"), annotation_matrix),
 optionalAnnotationsHeaders=c(slot(.Object1, "optionalAnnotationsHeaders"), AnnotationsHeaders)
)

return(GenomicRegions_data_object)

})


