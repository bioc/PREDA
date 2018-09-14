
##
## method GenomicRegionsFilter_neg: filter genomic regions to remove selected chromosomes
##

##
## GenomicRegions - GenomicRegionsFilter_neg
##

setMethod("GenomicRegionsFilter_neg", "GenomicRegions", function(.Object, chrToRemove, chrAsLabels=FALSE, quiet=FALSE) {


# extracting dataframe from GenomicRegions object
input_dataframe<-GenomicRegions2dataframe(.Object)


# extracting chromosomes List and labels
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")
selected_chromosomesLabels_list<-slot(.Object, "chromosomesLabels")
if (chrAsLabels) {
myChrSelection<-(!(selected_chromosomesLabels_list %in% chrToRemove))
} else {
myChrSelection<-(!(selected_chromosomes_list %in% chrToRemove))
}

# filtering chr lists and labels
selected_chromosomes_list<-selected_chromosomes_list[myChrSelection]
selected_chromosomesLabels_list<-selected_chromosomesLabels_list[myChrSelection]


if (length(selected_chromosomes_list)==0) {
  if (!quiet) {
  print("Empty chromosomes filter result")
  }
return(NULL)
}


# filtering dataframe
myChrSelection_selection<-(input_dataframe[,"chr"] %in% selected_chromosomes_list)
input_dataframe<-input_dataframe[myChrSelection_selection,]


#re-building GenomicRegions object from dataframe
if (is.null(input_dataframe$ids)) {
ids_column <-NULL
} else {
ids_column <- "ids"
}
GenomicRegions_data_object<-GenomicRegionsFromdataframe(GenomicRegions_dataframe=input_dataframe, ids_column=ids_column, chr_column="chr", start_column="start", end_column="end", chromosomesNumbers=selected_chromosomes_list, chromosomesLabels=selected_chromosomesLabels_list, chromosomesLabelsInput=NULL)

return(GenomicRegions_data_object)
})




