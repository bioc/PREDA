


setMethod("SODEGIR_GEstatistics", "ExpressionSet", function(.Object, pData_classColumn=NULL, referenceGroupLabel=NULL, statisticType=c("tstatistic", "FC", "FCmedian", "eBayes", "SAM"), singleSampleOutput=TRUE, varianceAll=FALSE) {

ExpressionSet_input<-.Object
# possibili valori per statisticType
# ("tstatistic", "FC", "FCmedian", "eBayes", "SAM")
Classes_vector<-pData(ExpressionSet_input)[,pData_classColumn]
Classes_vector<-as.character(Classes_vector)

  # check if we have two and only two sample classes
  if (length(unique(Classes_vector)) != 2) {
  stop("This function was developed to manage samples pertaining just to 2 classes!!")
  }

treatmentGroupLabel<-unique(Classes_vector[Classes_vector!=referenceGroupLabel])




    if (singleSampleOutput) {
        statisticsMatrix<-NULL
        
        if (varianceAll) {

          if (statisticType!="SAM") {
warning("varianceAll=TRUE in the single sample analysis (singleSampleOutput=TRUE)
is implemented only for SAM statistic type: as described in 
the original SODEGIR paper by Bicciato et al. (NAR 2009).
For other statistic types varianceAll=FALSE is used intead for singleSampleOutput")
          }
        ## statistics with variance computed on the whole set of normal and tumor samples
        ## as described in the paper by Bicciato et al.

        sam.data<-exprs(ExpressionSet_input)
        classFactor<-ifelse((Classes_vector==treatmentGroupLabel), yes=2, no=1)
        classFactor<-as.factor(classFactor)
        data<-list(x=sam.data,y=classFactor, geneid=rownames(sam.data),genenames=rownames(sam.data), logged2=TRUE)
        sam.obj<-samr(data,resp.type="Two class unpaired",nperms=1)
        meansReference<-rowMeans(sam.data[,(classFactor==1),drop=FALSE])
        statisticsMatrix<-((sam.data[,(classFactor==2),drop=FALSE]-meansReference)/sam.obj$sd)

        } else {
        # to compute the statistics and astimating variance only from the reference groups of samples
        # this is basically done by relicating each trated sample and using the standard functions for the statistics

          for (treatedSample in which(Classes_vector==treatmentGroupLabel)) {
          
          samples_selection<-c(which(Classes_vector==referenceGroupLabel), treatedSample, treatedSample)
          
          current_ExpressionSet_input<-ExpressionSet_input[,samples_selection]
          
          test_statistic<-GE_computeStatistic(current_ExpressionSet_input, pData_classColumn=pData_classColumn, statisticType=statisticType, referenceGroupLabel=referenceGroupLabel)
          
          statisticsMatrix<-cbind(statisticsMatrix, test_statistic)
          }
        }
        colnames(statisticsMatrix)<-sampleNames(ExpressionSet_input)[which(Classes_vector==treatmentGroupLabel)]
    } else {
    test_statistic<-GE_computeStatistic(ExpressionSet_input, pData_classColumn=pData_classColumn, statisticType=statisticType, referenceGroupLabel=referenceGroupLabel)
    statisticsMatrix<-matrix(test_statistic, ncol=1)
    colnames(statisticsMatrix)<-paste(treatmentGroupLabel, "VS", referenceGroupLabel, sep="_")
    }


rownames(statisticsMatrix)<-featureNames(ExpressionSet_input)
statisticsMatrix<-data.frame("probeIDS"=rownames(statisticsMatrix),statisticsMatrix, stringsAsFactors=FALSE)

return(statisticsMatrix)


})


