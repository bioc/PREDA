###
### This function merges a StatisticsForPREDA and a GenomicAnnotationsForPREDA object into a DataForPREDA object
###

MergeStatisticAnnotations2DataForPREDA<-function(StatisticsForPREDAObject, GenomicAnnotationsForPREDAObject, sortAndCleanNA=FALSE, quiet=FALSE, MedianCenter=FALSE) {

   if (class(StatisticsForPREDAObject) != "StatisticsForPREDA") {
   stop("StatisticsForPREDAObject is not an object of class StatisticsForPREDA.")
   }

   if (class(GenomicAnnotationsForPREDAObject) != "GenomicAnnotationsForPREDA") {
   stop("GenomicAnnotationsForPREDAObject is not an object of class GenomicAnnotationsForPREDA.")
   }


if (sortAndCleanNA) {
GenomicAnnotationsForPREDAObject<-GenomicAnnotationsSortAndCleanNA(GenomicAnnotationsForPREDAObject)
}

ids_from_annotations<-slot(GenomicAnnotationsForPREDAObject, "ids")
ids_from_statistics<-slot(StatisticsForPREDAObject, "ids")

  ids_from_annotations_common<-(ids_from_annotations %in% ids_from_statistics)
  ids_from_statistics_common<-(ids_from_statistics %in% ids_from_annotations)

  if (sum(ids_from_annotations_common)==0) {
  stop("The annotations and statistic can\'t be merged because no common ids were identified")
  } else if (sum(ids_from_annotations_common)<=3) {
  stop("The annotations and statistic can\'t be merged because no more than 3 common ids were identified")
  }

  if (sum(!(ids_from_statistics_common))>0) {
    if (!quiet) {
    cat(paste("\n",sum(!(ids_from_statistics_common)),"ids from the StatisticsForPREDA object have no corresponding entries in the GenomicAnnotations object.\n"))
    }
  }



StatisticsForPREDA_dataframe<-StatisticsForPREDA2dataframe(StatisticsForPREDAObject)

GenomicAnnotationsForPREDA_dataframe<-GenomicAnnotationsForPREDA2dataframe(GenomicAnnotationsForPREDAObject)



merged_dataframes<-merge(x=GenomicAnnotationsForPREDA_dataframe, y=StatisticsForPREDA_dataframe, by.x="ids", by.y="ids", all=FALSE, sort = FALSE)



chromosomesNumbers<-slot(GenomicAnnotationsForPREDAObject, "chromosomesNumbers")
chromosomesLabels<-slot(GenomicAnnotationsForPREDAObject, "chromosomesLabels")

remaining_chromosomes<-(chromosomesNumbers %in% unique(merged_dataframes[,"chr"]))
chromosomesNumbers<-chromosomesNumbers[remaining_chromosomes]
chromosomesLabels<-chromosomesLabels[remaining_chromosomes]





GenomicAnnotationsObject<-GenomicAnnotationsFromdataframe(
GenomicAnnotations_dataframe=merged_dataframes,
ids_column="ids",
chr_column="chr",
start_column="start",
end_column="end",
strand_column="strand",
chromosomesNumbers=chromosomesNumbers,
chromosomesLabels=chromosomesLabels,
chromosomesLabelsInput=NULL,
MinusStrandString="-1",
PlusStrandString="1",
optionalAnnotationsColumns=slot(GenomicAnnotationsForPREDAObject,"optionalAnnotationsHeaders"))


## crea funzione per fare DataFromPREDA object from a dataframe..... (??) --- quite useless

 DataForPREDA_object<-new("DataForPREDA",
 ids=slot(GenomicAnnotationsObject,"ids"),
 chr=slot(GenomicAnnotationsObject,"chr"),
 start=slot(GenomicAnnotationsObject,"start"),
 end=slot(GenomicAnnotationsObject,"end"),
 strand=slot(GenomicAnnotationsObject,"strand"),
 position=merged_dataframes[,"position"],
 optionalAnnotationsHeaders=slot(GenomicAnnotationsObject,"optionalAnnotationsHeaders"),
 optionalAnnotations=slot(GenomicAnnotationsObject,"optionalAnnotations"),
 statistic=as.matrix(merged_dataframes[,slot(StatisticsForPREDAObject, "analysesNames")]),
 analysesNames=slot(StatisticsForPREDAObject, "analysesNames"),
 testedTail=slot(StatisticsForPREDAObject, "testedTail"),
 chromosomesNumbers=slot(GenomicAnnotationsObject,"chromosomesNumbers"),
 chromosomesLabels=slot(GenomicAnnotationsObject,"chromosomesLabels")
 )

  # scale median statistics values to zero
  if (MedianCenter) {
  DataForPREDA_object<-DataForPREDAMedianCenter(DataForPREDA_object)
  }

return(DataForPREDA_object)

}





