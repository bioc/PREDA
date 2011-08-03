
####
#### statisticsForPREDAfromEset : function for computing a statisticForPREDA object from an expression set
####


setMethod("statisticsForPREDAfromEset", "ExpressionSet", function(.Object, pData_classColumn=NULL, statisticType=NULL, logged=TRUE, referenceGroupLabel=NULL, classVector=NULL, testedTail="both") {


ExpressionSet_input<-.Object

  if (is.null(classVector)) {
    if ((is.null(pData_classColumn)) | (!(pData_classColumn %in% colnames(pData(.Object))))) {
    stop("Please specify a valid phenodata column containing sample classes")
    }
    # extract information about sample classes
    classFactor<-as.factor(pData(.Object)[,pData_classColumn])
  } else {
    if (length(classVector) != length(sampleNames(ExpressionSet_input))) {
    stop("Class vector length is not equal to the number of samples in the input ExpressionSet")
    }
    classFactor<-as.factor(classVector)
  }

Classes_vector<-as.character(classFactor)
treatmentGroupLabel<-unique(Classes_vector[Classes_vector!=referenceGroupLabel])

    statisticsMatrix<-NULL
    for (treatmentGroup in treatmentGroupLabel) {
    samples_selection<-c(which(Classes_vector==referenceGroupLabel), which(Classes_vector==treatmentGroup))

    current_ExpressionSet_input<-ExpressionSet_input[,samples_selection]
    current_Classes_vector_input<-Classes_vector[samples_selection]
    test_statistic<-GE_computeStatistic(current_ExpressionSet_input, classVector=current_Classes_vector_input, statisticType=statisticType, logged=logged, referenceGroupLabel=referenceGroupLabel)
    statisticsMatrix<-cbind(statisticsMatrix, test_statistic)
    }
    colnames(statisticsMatrix)<-paste(treatmentGroupLabel, "VS", referenceGroupLabel, sep="_")

rownames(statisticsMatrix)<-featureNames(ExpressionSet_input)
statisticsMatrix<-data.frame("probeIDS"=rownames(statisticsMatrix),statisticsMatrix, stringsAsFactors=FALSE)


## converto le statistiche in un oggetto StatisticsForPREDA
StatisticsForPREDA_object<-StatisticsForPREDAFromdataframe(StatisticsForPREDA_dataframe=statisticsMatrix, ids_column=1, statistic_columns=2:ncol(statisticsMatrix), testedTail=testedTail)


})


