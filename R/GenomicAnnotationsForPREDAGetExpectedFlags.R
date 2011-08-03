

setMethod("GenomicAnnotationsForPREDAGetExpectedFlags", "GenomicAnnotationsForPREDA", function(.Object, genomicRegionsList=NULL, null.value=0, significant.value=1) {

  if (class(genomicRegionsList) != "list") {
  stop("A list of GenomicRegions objects is required as input")
  }

    check_classes<-sapply(genomicRegionsList, class)
    if (!all(check_classes %in% c("GenomicRegions", "NULL"))) {
    stop("genomicRegionsList must contain only GenomicRegions objects")
    }

ExpectedFlags.matrix<-matrix(null.value, nrow=length(slot(.Object,"ids")), ncol=length(genomicRegionsList))


  for (genomicRegionsIndex in 1:length(genomicRegionsList)) {
    currentgenomicRegionsObject<-genomicRegionsList[[genomicRegionsIndex]]
    if (!is.null(currentgenomicRegionsObject)) {
        for (region.index in 1:GenomicRegionsNumber(currentgenomicRegionsObject)) {
            chr<-slot(currentgenomicRegionsObject, "chr")[region.index]
            start<-slot(currentgenomicRegionsObject, "start")[region.index]
            end<-slot(currentgenomicRegionsObject, "end")[region.index]
            feature_selection<-which((slot(.Object, "chr")==chr) & (slot(.Object, "position")>=start) & (slot(.Object, "position")<=end))
            ExpectedFlags.matrix[feature_selection, genomicRegionsIndex]<-significant.value
        }
    }
  }

colnames(ExpectedFlags.matrix)<-names(genomicRegionsList)
rownames(ExpectedFlags.matrix)<-slot(.Object, "ids")

return(ExpectedFlags.matrix)

})

