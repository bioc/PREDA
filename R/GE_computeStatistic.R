

####
#### GE_computeStatistic : function for computing a statistic for differential expression from an expression set
####


setMethod("GE_computeStatistic", "ExpressionSet", function(.Object, pData_classColumn=NULL, statisticType=NULL, logged=TRUE, referenceGroupLabel=NULL, classVector=NULL) {

# require(Biobase)
  if (is.null(classVector)) {
    if ((is.null(pData_classColumn)) | (!(pData_classColumn %in% colnames(pData(.Object))))) {
    stop("Please specify a valid phenodata column containing sample classes")
    }
  # extract information about sample classes
  classFactor<-as.factor(pData(.Object)[,pData_classColumn])
  } else {
    if (length(classVector) != length(sampleNames(.Object))) {
    stop("Class vector length is not equal to the number of samples in the input ExpressionSet")
    }
    classFactor<-as.factor(classVector)
  }



  # observed data matrix
  datamatrix<-exprs(.Object)

  # using service function for this step
  statistic_vector<-GE_computeStatistic_onMatrix(datamatrix=datamatrix, classFactor=classFactor, statisticType=statisticType, logged=logged, referenceGroupLabel=referenceGroupLabel)

return(statistic_vector)

})





###
### funzione accessoria per GE_computeStatistic
###

GE_computeStatistic_onMatrix<-function(datamatrix, classFactor, statisticType, logged=TRUE, referenceGroupLabel=NULL) {


  # we must work on logged data
  if (!logged) {
  warning("Data now are Log2 transformed!")
  datamatrix<-log2(datamatrix)
  }

  implemented_statistics<-c("tstatistic", "FC", "FCmedian", "eBayes") #, "SAM")

  if (!(statisticType %in% implemented_statistics)) {
  stop(paste("The",statisticType, "is not implemented"))
  }

  classFactor<-as.factor(classFactor)

  # check if we have two and only two sample classes
  if (length(levels(classFactor)) != 2) {
  stop("This function was developed to manage samples pertaining just to 2 classes!!")
  }

    if (!is.null(referenceGroupLabel)) {
    classFactor<-ifelse((as.character(classFactor)==referenceGroupLabel), yes=1, no=2)
    classFactor<-as.factor(classFactor)
    }


  # computing the statistic
  if (statisticType=="tstatistic") {
    statistic_vector<-apply(datamatrix,1, FUN=function(x, group1=which(classFactor==levels(classFactor)[1]), group2=which(classFactor==levels(classFactor)[2])) {
    statistic<-t.test(x=x[group2], y = x[group1])$statistic
    return(statistic)
    })
  } else if (statisticType=="FC") {
    statistic_vector<-apply(datamatrix,1, FUN=function(x, group1=which(classFactor==levels(classFactor)[1]), group2=which(classFactor==levels(classFactor)[2])) {
    statistic<- (mean(x[group2]) - mean(x[group1]))
    return(statistic)
    })
  } else if (statisticType=="FCmedian") {
    statistic_vector<-apply(datamatrix,1, FUN=function(x, group1=which(classFactor==levels(classFactor)[1]), group2=which(classFactor==levels(classFactor)[2])) {
    statistic<- (median(x[group2]) - median(x[group1]))
    return(statistic)
    })
  } else if (statisticType=="eBayes") {
    require(limma)
    design<-model.matrix(~0+classFactor)
    fit<-lmFit(datamatrix, design)

      mycontrasts<-paste(
      paste("classFactor",levels(classFactor)[2],sep=""),
      "-",
      paste("classFactor",levels(classFactor)[1],sep=""),
      sep="")

    names(mycontrasts)<-paste("contrast",levels(classFactor)[2],"vs", levels(classFactor)[1],sep="_")

    contrast.matrix<-makeContrasts(
    contrasts=mycontrasts,
    levels=design
    )
    colnames(contrast.matrix)<-names(mycontrasts)
    fit2<-contrasts.fit(fit, contrast.matrix)
    fit3<-eBayes(fit2)

    statistic_vector<-topTable(fit3, coef=1, number=nrow(fit3$genes), adjust.method="none", sort.by="none")$t
#  } else if (statisticType=="SAM") {
#    require(samr)
#   sam.dataobject<-list(x=datamatrix,y=classFactor, geneid=rownames(datamatrix), genenames=rownames(datamatrix), logged2=TRUE)
#    samr.obj<-samr(data=sam.dataobject, resp.type="Two class unpaired", nperms=1, testStatistic="standard")
#    delta.table <- samr.compute.delta.table(samr.obj,nvals=1)
#    siggenes.table<-samr.compute.siggenes.table(samr.obj=samr.obj, del=delta.table[1,"delta"],  data=sam.dataobject, delta.table=delta.table, all.genes=TRUE)
#    outputTable<-rbind(siggenes.table$genes.up, siggenes.table$genes.lo)
#    rownames(outputTable)<-outputTable[,"Gene ID"]
#    statistic_vector<-as.numeric(outputTable[rownames(datamatrix),"Score(d)"])

  } else {
  stop("Wrong statisticType")
  }

return(statistic_vector)


}

