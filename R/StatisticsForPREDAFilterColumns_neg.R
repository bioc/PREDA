##
## Method StatisticsForPREDAFilterColumns_neg to filter analyses to remove selected analyses
##

##
## StatisticsForPREDA - StatisticsForPREDAFilterColumns_neg
##

setMethod("StatisticsForPREDAFilterColumns_neg", "StatisticsForPREDA", function(.Object, analysesToRemove, analysesAsNames=FALSE) {

# extracting chromosomes List and labels
selected_analysesNames_list<-slot(.Object, "analysesNames")
selected_analyses_list<-(1:length(selected_analysesNames_list))

if (analysesAsNames) {
mySelection<-(!(selected_analysesNames_list %in% analysesToRemove))
} else {
mySelection<-(!(selected_analyses_list %in% analysesToRemove))
}

# filtering chr lists and labels
selected_analyses_list<-selected_analyses_list[mySelection]
selected_analysesNames_list<-selected_analysesNames_list[mySelection]


if (length(selected_analysesNames_list)==0) {
stop("Empty statistics filter result")
}

# filtering statistic matrix
statistics<-slot(.Object,"statistic")
statistics<-statistics[,selected_analyses_list]

StatisticsForPREDA_object<-new("StatisticsForPREDA", ids=slot(.Object,"ids"), statistic=statistics, analysesNames=selected_analysesNames_list, testedTail=slot(.Object,"testedTail"))

return(StatisticsForPREDA_object)

})


##
## DataForPREDA - StatisticsForPREDAFilterColumns_neg
##

setMethod("StatisticsForPREDAFilterColumns_neg", "DataForPREDA", function(.Object, analysesToRemove, analysesAsNames=FALSE) {

# extracting statistics for PREDA
StatisticsForPREDA_object<-DataForPREDA2StatisticsForPREDA(.Object)

# extracting genomic annotations
GenomicAnnotationsForPREDA_object<-DataForPREDA2GenomicAnnotationsForPREDA(.Object)

# filter statistics
StatisticsForPREDA_object<-StatisticsForPREDAFilterColumns_neg(StatisticsForPREDA_object, analysesToRemove=analysesToRemove, analysesAsNames=analysesAsNames)

# merging filtered data
DataForPREDA_object<-MergeStatisticAnnotations2DataForPREDA(StatisticsForPREDAObject=StatisticsForPREDA_object, GenomicAnnotationsForPREDAObject=GenomicAnnotationsForPREDA_object)


return(DataForPREDA_object)

})

