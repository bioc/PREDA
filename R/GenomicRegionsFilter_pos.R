##
## method GenomicRegionsFilter_pos: filter genomic regions to KEEP selected chromosomes
##

##
## GenomicRegions - GenomicRegionsFilter_pos
##

setMethod("GenomicRegionsFilter_pos", "GenomicRegions", function(.Object, chrToRetain, chrAsLabels=FALSE, quiet=FALSE) {



# extracting chromosomes List and labels
if (chrAsLabels) {
selected_chromosomesLabels_list<-slot(.Object, "chromosomesLabels")
myChrSelection<-(!(selected_chromosomesLabels_list %in% chrToRetain))
chrToRemove<-selected_chromosomesLabels_list[myChrSelection]
} else {
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")
myChrSelection<-(!(selected_chromosomes_list %in% chrToRetain))
chrToRemove<-selected_chromosomes_list[myChrSelection]
}

# applying negative filtering
GenomicRegions_data_object<-GenomicRegionsFilter_neg(.Object , chrToRemove=chrToRemove, chrAsLabels=chrAsLabels, quiet=quiet)

return(GenomicRegions_data_object)

})


