

####
#### eset2GenomicAnnotations : function to extract genomic annotations from annotations library associated to an expressionset
####


setMethod("eset2GenomicAnnotations", "ExpressionSet", function(.Object, retain.chrs=NULL, optionalAnnotations=NULL) {


###############################################################################
###############################################################################
## extract GenomicAnnotations
probesets<- featureNames(.Object)

GenomicsAnnotations_object<-GenomicAnnotationsFromLibrary(annotLibrary=annotation(.Object), probeIDs=probesets, retain.chrs=retain.chrs, optionalAnnotations=optionalAnnotations)

return(GenomicsAnnotations_object)


})



