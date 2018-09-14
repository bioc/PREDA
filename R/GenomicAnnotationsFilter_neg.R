##
## method GenomicAnnotationsFilter_neg to filter annotations to remove selected chromosomes
##


##
## GenomicAnnotations - GenomicAnnotationsFilter_neg
##

setMethod("GenomicAnnotationsFilter_neg", "GenomicAnnotations", function(.Object, chrToRemove, chrAsLabels=FALSE) {

# extracting dataframe from GenomicAnnotations object
input_dataframe<-GenomicAnnotations2dataframe(.Object)

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
stop("Empty chromosomes filter result")
}


# filtering dataframe
myChrSelection_selection<-(input_dataframe[,"chr"] %in% selected_chromosomes_list)
input_dataframe<-input_dataframe[myChrSelection_selection,]


#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=selected_chromosomes_list, chromosomesLabels=selected_chromosomesLabels_list, chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

return(GenomicAnnotations_data_object)
})



##
## GenomicAnnotationsForPREDA - GenomicAnnotationsFilter_neg
##

setMethod("GenomicAnnotationsFilter_neg", "GenomicAnnotationsForPREDA", function(.Object, chrToRemove, chrAsLabels=FALSE) {

# extracting dataframe from GenomicAnnotations object
input_dataframe<-GenomicAnnotations2dataframe(.Object)

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
stop("Empty chromosomes filter result")
}


# filtering dataframe
myChrSelection_selection<-(input_dataframe[,"chr"] %in% selected_chromosomes_list)
input_dataframe<-input_dataframe[myChrSelection_selection,]


#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=selected_chromosomes_list, chromosomesLabels=selected_chromosomesLabels_list, chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

# re-building GenomicAnnotationsForPREDA object
positions<-input_dataframe[,"position"]
GenomicAnnotationsForPREDA_object<-GenomicAnnotations2GenomicAnnotationsForPREDA(GenomicAnnotations_data_object, positions=positions)

return(GenomicAnnotationsForPREDA_object)
})


##
## DataForPREDA - GenomicAnnotationsFilter_neg
##

setMethod("GenomicAnnotationsFilter_neg", "DataForPREDA", function(.Object, chrToRemove, chrAsLabels=FALSE) {


# extracting statistics for PREDA
StatisticsForPREDA_object<-DataForPREDA2StatisticsForPREDA(.Object)

# extracting genomic annotations
GenomicAnnotationsForPREDA_object<-DataForPREDA2GenomicAnnotationsForPREDA(.Object)

# sort and clean annotations
GenomicAnnotationsForPREDA_object<-GenomicAnnotationsFilter_neg(GenomicAnnotationsForPREDA_object, chrToRemove=chrToRemove, chrAsLabels=chrAsLabels)

# merging filtered data
DataForPREDA_object<-MergeStatisticAnnotations2DataForPREDA(StatisticsForPREDAObject=StatisticsForPREDA_object, GenomicAnnotationsForPREDAObject=GenomicAnnotationsForPREDA_object, quiet=TRUE)

return(DataForPREDA_object)

})
