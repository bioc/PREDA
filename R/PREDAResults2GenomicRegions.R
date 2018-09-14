
##
## method PREDAResults2GenomicRegions
##

##
## PREDAResults - PREDAResults2GenomicRegions
##

setMethod("PREDAResults2GenomicRegions", "PREDAResults",
function(.Object, qval.threshold=0.05,
use.referencePositions=TRUE,  smoothStatistic.tail=NULL,
smoothStatistic.threshold=NULL) {

include.genes.number<-FALSE
keep.annotation<-NULL

available_analyses<-slot(.Object, "analysesNames")

genomic_regions_list<-list()
  for (current_analysis in available_analyses) {
  current_genomic_regions<-PREDAResults2GenomicRegionsSingle(.Object, qval.threshold=qval.threshold, analysisName=current_analysis, use.referencePositions=use.referencePositions,  smoothStatistic.tail=smoothStatistic.tail, smoothStatistic.threshold=smoothStatistic.threshold)
  genomic_regions_list<-c(genomic_regions_list, list(current_genomic_regions))
  }

names(genomic_regions_list)<-available_analyses

return(genomic_regions_list)

})

