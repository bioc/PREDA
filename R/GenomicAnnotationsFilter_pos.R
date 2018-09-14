##
## Method GenomicAnnotationsFilter_pos to filter annotations to KEEP selected chromosomes
##

##
## GenomicAnnotations - GenomicAnnotationsFilter_pos
##

setMethod("GenomicAnnotationsFilter_pos", "GenomicAnnotations", function(.Object, chrToRetain, chrAsLabels=FALSE) {

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
GenomicAnnotations_data_object<-GenomicAnnotationsFilter_neg(.Object , chrToRemove=chrToRemove, chrAsLabels=chrAsLabels)

return(GenomicAnnotations_data_object)

})


##
## GenomicAnnotationsForPREDA - GenomicAnnotationsFilter_pos
##

setMethod("GenomicAnnotationsFilter_pos", "GenomicAnnotationsForPREDA", function(.Object, chrToRetain, chrAsLabels=FALSE) {

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
GenomicAnnotationsForPREDA_object<-GenomicAnnotationsFilter_neg(.Object , chrToRemove=chrToRemove, chrAsLabels=chrAsLabels)

return(GenomicAnnotationsForPREDA_object)

})


##
## DataForPREDA - GenomicAnnotationsFilter_pos
##

setMethod("GenomicAnnotationsFilter_pos", "DataForPREDA", function(.Object, chrToRetain, chrAsLabels=FALSE) {

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
DataForPREDA_object<-GenomicAnnotationsFilter_neg(.Object , chrToRemove=chrToRemove, chrAsLabels=chrAsLabels)

return(DataForPREDA_object)

})


