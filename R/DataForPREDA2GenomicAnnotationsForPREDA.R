
##
## Method DataForPREDA2GenomicAnnotationsForPREDA is used to extract the GenomicAnnotationsForPREDA object from the DataForPREDA object
##

##
## DataForPREDA - DataForPREDA2GenomicAnnotationsForPREDA
##

setMethod("DataForPREDA2GenomicAnnotationsForPREDA", "DataForPREDA", function(.Object) {

GenomicAnnotationsForPREDA_object<-new("GenomicAnnotationsForPREDA", ids=slot(.Object,"ids"), chr=slot(.Object,"chr"), start=slot(.Object,"start"), end=slot(.Object,"end"), strand=slot(.Object,"strand"), position=slot(.Object,"position"), chromosomesNumbers=slot(.Object,"chromosomesNumbers"), chromosomesLabels=slot(.Object,"chromosomesLabels"), optionalAnnotations=slot(.Object,"optionalAnnotations"), optionalAnnotationsHeaders=slot(.Object,"optionalAnnotationsHeaders"))

return(GenomicAnnotationsForPREDA_object)

})


