##
## GenomicAnnotations - GenomicAnnotationsExtract
##

setMethod("GenomicAnnotationsExtract", "GenomicAnnotations", function(.Object, chr, start, end, AnnotationsHeader=NULL, sep.character="; ", complete.inclusion=FALSE, skipSorting=FALSE, annotationAsRange=FALSE, getJustFeaturesNumber=FALSE) {

    if (is.null(AnnotationsHeader) & (!getJustFeaturesNumber)) {
    return(NULL)
    } else if (!(is.null(AnnotationsHeader) & (getJustFeaturesNumber))) {
    # exception if no annotations are required as output but just the number of features
        if ((!is.character(AnnotationsHeader)) | (length(AnnotationsHeader) > 1)) {
        stop("Please specify one single annotation field using the corrisponding header name")
        } else if (!(AnnotationsHeader %in% slot(.Object, "optionalAnnotationsHeaders"))) {
        stop(paste("The selected header name \"", AnnotationsHeader,"\" is not among available annotation fields."))
        }
    }

if (!skipSorting) {
.Object<-GenomicAnnotationsSortAndCleanNA(.Object)
}


GenomicAnnotations_selected_chromosome<-GenomicAnnotationsFilter_pos(.Object, chrToRetain=chr)


feature_selection.lower_bound<-slot(GenomicAnnotations_selected_chromosome, ifelse(complete.inclusion, yes="start", no="end"))
feature_selection.upper_bound<-slot(GenomicAnnotations_selected_chromosome, ifelse(complete.inclusion, yes="end", no="start"))
feature_selection <- which((feature_selection.lower_bound >= start) & (feature_selection.upper_bound <= end))

    if (getJustFeaturesNumber) {
    selected_feature_annotations<-length(feature_selection)
    } else {
            if (annotationAsRange) {
            feature_selection <- range(feature_selection)
            }
        selected_feature_annotations<-slot(GenomicAnnotations_selected_chromosome,"optionalAnnotations")[feature_selection, which(slot(.Object, "optionalAnnotationsHeaders")==AnnotationsHeader)]
        selected_feature_annotations<-paste(selected_feature_annotations, collapse= sep.character)
    }

return(selected_feature_annotations)

})

