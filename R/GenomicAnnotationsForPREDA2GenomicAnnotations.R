
##
## GenomicAnnotationsForPREDA - GenomicAnnotationsForPREDA2GenomicAnnotations
##
## Method to extract the GenomicAnnotations object from the GenomicAnnotationsForPREDA object
##

setMethod("GenomicAnnotationsForPREDA2GenomicAnnotations", "GenomicAnnotationsForPREDA", function(.Object) {

GenomicAnnotations_object<-new("GenomicAnnotations", ids=slot(.Object,"ids"), chr=slot(.Object,"chr"), start=slot(.Object,"start"), end=slot(.Object,"end"), strand=slot(.Object,"strand"), chromosomesNumbers=slot(.Object,"chromosomesNumbers"), chromosomesLabels=slot(.Object,"chromosomesLabels"), optionalAnnotations=slot(.Object,"optionalAnnotations"), optionalAnnotationsHeaders=slot(.Object,"optionalAnnotationsHeaders"))

return(GenomicAnnotations_object)

})

