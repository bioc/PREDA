##
## method GenomicAnnotationsSortAndCleanNA to sort annotations according to selected chromosomes and to remove genes containing any NA annotation field
##

##
## GenomicAnnotations - GenomicAnnotationsSortAndCleanNA
##

setMethod("GenomicAnnotationsSortAndCleanNA", "GenomicAnnotations", function(.Object, sorting_position_column="start") {

# extracting dataframe from GenomicAnnotations object
input_dataframe<-GenomicAnnotations2dataframe(.Object)
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")

# cleaning NAs
input_dataframe<-CleanNAAnnotationDataframe(input_dataframe=input_dataframe, selected_columns_list=c("chr", "start", "end", "strand"))

# sorting annotations
input_dataframe<-SortAnnotationDataframe(input_dataframe=input_dataframe, chromosomes_column="chr",selected_chromosomes_list=selected_chromosomes_list, sorting_position_column=sorting_position_column)

#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=slot(.Object, "chromosomesNumbers"), chromosomesLabels=slot(.Object, "chromosomesLabels"), chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

return(GenomicAnnotations_data_object)
})


##
## GenomicAnnotationsForPREDA - GenomicAnnotationsSortAndCleanNA
##

setMethod("GenomicAnnotationsSortAndCleanNA", "GenomicAnnotationsForPREDA", function(.Object, sorting_position_column="position") {


# extracting dataframe from GenomicAnnotationsForPREDA object
input_dataframe<-GenomicAnnotations2dataframe(.Object)
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")

# cleaning NAs
input_dataframe<-CleanNAAnnotationDataframe(input_dataframe=input_dataframe, selected_columns_list=c("chr", "start", "end", "strand"))

# sorting annotations
input_dataframe<-SortAnnotationDataframe(input_dataframe=input_dataframe, chromosomes_column="chr",selected_chromosomes_list=selected_chromosomes_list, sorting_position_column=sorting_position_column)

#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=slot(.Object, "chromosomesNumbers"), chromosomesLabels=slot(.Object, "chromosomesLabels"), chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

# re-building GenomicAnnotationsForPREDA object
positions<-input_dataframe[,"position"]
GenomicAnnotationsForPREDA_object<-GenomicAnnotations2GenomicAnnotationsForPREDA(GenomicAnnotations_data_object, positions=positions)

return(GenomicAnnotationsForPREDA_object)
})


##
## DataForPREDA - GenomicAnnotationsSortAndCleanNA
##

setMethod("GenomicAnnotationsSortAndCleanNA", "DataForPREDA", function(.Object, sorting_position_column="position") {

# extracting statistics for PREDA
StatisticsForPREDA_object<-DataForPREDA2StatisticsForPREDA(.Object)

# extracting genomic annotations
GenomicAnnotationsForPREDA_object<-DataForPREDA2GenomicAnnotationsForPREDA(.Object)

# sort and clean annotations
GenomicAnnotationsForPREDA_object<-GenomicAnnotationsSortAndCleanNA(GenomicAnnotationsForPREDA_object, sorting_position_column=sorting_position_column)

# merging filtered data
DataForPREDA_object<-MergeStatisticAnnotations2DataForPREDA(StatisticsForPREDAObject=StatisticsForPREDA_object, GenomicAnnotationsForPREDAObject=GenomicAnnotationsForPREDA_object)


return(DataForPREDA_object)

})



##
## PREDAResults - GenomicAnnotationsSortAndCleanNA
##

setMethod("GenomicAnnotationsSortAndCleanNA", "PREDAResults", function(.Object, sorting_position_column="position") {

# extracting dataframe from PREDAResults object
input_dataframe<-PREDAResults2dataframe(.Object)
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")

# cleaning NAs
input_dataframe<-CleanNAAnnotationDataframe(input_dataframe=input_dataframe, selected_columns_list=c("chr", "start", "end", "strand"))

# sorting annotations
input_dataframe<-SortAnnotationDataframe(input_dataframe=input_dataframe, chromosomes_column="chr",selected_chromosomes_list=selected_chromosomes_list, sorting_position_column=sorting_position_column)


#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=slot(.Object, "chromosomesNumbers"), chromosomesLabels=slot(.Object, "chromosomesLabels"), chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

# re-building GenomicAnnotationsForPREDA object
positions<-input_dataframe[,"position"]
GenomicAnnotationsForPREDA_object<-GenomicAnnotations2GenomicAnnotationsForPREDA(GenomicAnnotations_data_object, positions=positions)


analysesNames <- slot(.Object, "analysesNames")
testedTail <- slot(.Object, "testedTail")

smoothStatistic <- as.matrix(input_dataframe[,paste("smoothStatistic", analysesNames, sep=".")])
pvalue <- as.matrix(input_dataframe[,paste("pvalue", analysesNames, sep=".")])
qvalue <- as.matrix(input_dataframe[,paste("qvalue", analysesNames, sep=".")])


PREDAResults_data_object<-new("PREDAResults",
 ids=slot(GenomicAnnotationsForPREDA_object, "ids"),
 chr=slot(GenomicAnnotationsForPREDA_object, "chr"),
 start=slot(GenomicAnnotationsForPREDA_object, "start"),
 end=slot(GenomicAnnotationsForPREDA_object, "end"),
 strand=slot(GenomicAnnotationsForPREDA_object, "strand"),
 position=slot(GenomicAnnotationsForPREDA_object, "position"),
 chromosomesNumbers=slot(GenomicAnnotationsForPREDA_object, "chromosomesNumbers"),
 chromosomesLabels=slot(GenomicAnnotationsForPREDA_object, "chromosomesLabels"),
 optionalAnnotations=slot(GenomicAnnotationsForPREDA_object, "optionalAnnotations"),
 optionalAnnotationsHeaders=slot(GenomicAnnotationsForPREDA_object, "optionalAnnotationsHeaders"),

 analysesNames=analysesNames,
 testedTail=testedTail,
 smoothStatistic=smoothStatistic,
 pvalue=pvalue,
 qvalue=qvalue
)


return(PREDAResults_data_object)

})



##
## PREDADataAndResults - GenomicAnnotationsSortAndCleanNA
##

setMethod("GenomicAnnotationsSortAndCleanNA", "PREDADataAndResults", function(.Object, sorting_position_column="position") {

# extracting dataframe from PREDADataAndResults object
input_dataframe<-PREDADataAndResults2dataframe(.Object)
selected_chromosomes_list<-slot(.Object, "chromosomesNumbers")

# cleaning NAs
input_dataframe<-CleanNAAnnotationDataframe(input_dataframe=input_dataframe, selected_columns_list=c("chr", "start", "end", "strand"))

# sorting annotations
input_dataframe<-SortAnnotationDataframe(input_dataframe=input_dataframe, chromosomes_column="chr",selected_chromosomes_list=selected_chromosomes_list, sorting_position_column=sorting_position_column)


#re-building GenomicAnnotations object from dataframe
GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=input_dataframe, ids_column="ids", chr_column="chr", start_column="start", end_column="end", strand_column="strand", chromosomesNumbers=slot(.Object, "chromosomesNumbers"), chromosomesLabels=slot(.Object, "chromosomesLabels"), chromosomesLabelsInput=NULL, MinusStrandString="-1", PlusStrandString="1", optionalAnnotationsColumns=slot(.Object, "optionalAnnotationsHeaders"))

# re-building GenomicAnnotationsForPREDA object
positions<-input_dataframe[,"position"]
GenomicAnnotationsForPREDA_object<-GenomicAnnotations2GenomicAnnotationsForPREDA(GenomicAnnotations_data_object, positions=positions)

analysesNames <- slot(.Object, "analysesNames")
testedTail <- slot(.Object, "testedTail")

statistic <- as.matrix(input_dataframe[,analysesNames])
smoothStatistic <- as.matrix(input_dataframe[,paste("smoothStatistic", analysesNames, sep=".")])
pvalue <- as.matrix(input_dataframe[,paste("pvalue", analysesNames, sep=".")])
qvalue <- as.matrix(input_dataframe[,paste("qvalue", analysesNames, sep=".")])


PREDADataAndResults_data_object<-new("PREDADataAndResults",
 ids=slot(GenomicAnnotationsForPREDA_object, "ids"),
 chr=slot(GenomicAnnotationsForPREDA_object, "chr"),
 start=slot(GenomicAnnotationsForPREDA_object, "start"),
 end=slot(GenomicAnnotationsForPREDA_object, "end"),
 strand=slot(GenomicAnnotationsForPREDA_object, "strand"),
 position=slot(GenomicAnnotationsForPREDA_object, "position"),
 chromosomesNumbers=slot(GenomicAnnotationsForPREDA_object, "chromosomesNumbers"),
 chromosomesLabels=slot(GenomicAnnotationsForPREDA_object, "chromosomesLabels"),
 optionalAnnotations=slot(GenomicAnnotationsForPREDA_object, "optionalAnnotations"),
 optionalAnnotationsHeaders=slot(GenomicAnnotationsForPREDA_object, "optionalAnnotationsHeaders"),

 statistic=statistic,
 analysesNames=analysesNames,
 testedTail=testedTail,
 smoothStatistic=smoothStatistic,
 pvalue=pvalue,
 qvalue=qvalue
)

return(PREDADataAndResults_data_object)


})

