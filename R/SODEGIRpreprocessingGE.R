

SODEGIRpreprocessingGE<-function(SampleInfoFile=NULL, CELfiles_dir=NULL, AffyBatchInput=NULL, custom_cdfname,
arrayNameColumn=NULL, sampleNameColumn=NULL, classColumn, referenceGroupLabel,
statisticType, optionalAnnotations=NULL, retain.chrs=NULL,
reference_position_type="median", testedTail="both", singleSampleOutput=TRUE, varianceAll=FALSE) {

# raw input data can be alternatively provided as an Affybatch object
# please note this function is a user-friendly preprocessing function for Affy gene expression microarrays
# step by step preprocessing functions can be used with any other platform
if (!is.null(AffyBatchInput)) {
  if (!(inherits(AffyBatchInput, what="AffyBatch"))) {
  stop("the object AffyBatchInput is not an object of class AffyBatch")
  }
} else {
  if (is.null(SampleInfoFile)) {
  stop("Please provide input data either as a sampleinfo file or as an AffyBatch object")
  }
  if (is.null(CELfiles_dir)) {
  CELfiles_dir<-getwd()
  }
}


## ESEGUO RMA NORMALIZATION CON CUSTOM CDF FILTRANDO I PROBESET CON MENO DI 4 PROBES
ExpressionSet_input<-RMAwithCDFfilter(
SampleInfoFile=SampleInfoFile,
CELfiles_dir=CELfiles_dir,
AffyBatchInput=AffyBatchInput,
custom_cdfname=custom_cdfname,
arrayNameColumn = arrayNameColumn,
sampleNameColumn = sampleNameColumn,
classColumn = classColumn
)



#### creo oggetto GenomicAnnotations
GenomicsAnnotations_object<-eset2GenomicAnnotations(ExpressionSet_input, retain.chrs=retain.chrs, optionalAnnotations=optionalAnnotations)



#### creo oggetto GenomicAnnotationsForPREDA
# decido quale posizione usare come riferimento nelle analisi PREDA
GenomicsAnnotationsForPREDA_object<-GenomicAnnotations2GenomicAnnotationsForPREDA(GenomicsAnnotations_object, reference_position_type=reference_position_type)

# ordino le annotazioni e rimuovo glio NA
# GenomicAnnotationsSortAndCleanNA : questa funzinoe pu essere usata sia su oggetti GenomicsAnnotations che  GenomicsAnnotationsForPREDA
GenomicsAnnotationsForPREDA_object<-GenomicAnnotationsSortAndCleanNA(GenomicsAnnotationsForPREDA_object)


### calcolo le statistiche SODEGIR per ogni singolo campione del gruppo "tumor"
statisticsMatrix<-SODEGIR_GEstatistics(ExpressionSet_input, pData_classColumn="Class", referenceGroupLabel=referenceGroupLabel, statisticType=statisticType, singleSampleOutput=singleSampleOutput, varianceAll=varianceAll)


## converto le statistiche in un oggetto StatisticsForPREDA
StatisticsForPREDA_object<-StatisticsForPREDAFromdataframe(StatisticsForPREDA_dataframe=statisticsMatrix, ids_column=1, statistic_columns=2:ncol(statisticsMatrix), testedTail=testedTail)

## unisco statistiche e annotazioni in un oggetto DataForPREDA
DataForPREDA_object<-MergeStatisticAnnotations2DataForPREDA(StatisticsForPREDA_object, GenomicsAnnotationsForPREDA_object, sortAndCleanNA=TRUE, quiet=TRUE)



return(DataForPREDA_object)


}


