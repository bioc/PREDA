##
## GenomicAnnotations - GenomicAnnotations2GenomicAnnotationsForPREDA
##
## Method for class GenomicAnnotations that is actually used only to generate a new GenomicAnnotationsForPREDA object
##

setMethod("GenomicAnnotations2GenomicAnnotationsForPREDA", "GenomicAnnotations", function(.Object, positions=NULL, reference_position_type=NULL) {

  if (is.null(positions)) {
  positions<-GenomicAnnotations2reference_positions(.Object, reference_position_type=reference_position_type, withnames=FALSE)
  }

GenomicAnnotationsForPREDA_object<-new("GenomicAnnotationsForPREDA", ids=slot(.Object,"ids"), chr=slot(.Object,"chr"), start=slot(.Object,"start"), end=slot(.Object,"end"), strand=slot(.Object,"strand"), chromosomesNumbers=slot(.Object,"chromosomesNumbers"), chromosomesLabels=slot(.Object,"chromosomesLabels"), position=positions, optionalAnnotations=slot(.Object,"optionalAnnotations"), optionalAnnotationsHeaders=slot(.Object,"optionalAnnotationsHeaders"))

return(GenomicAnnotationsForPREDA_object)

})

