##
## method getStatisticByName to retrieve each statistic column
##

##
## StatisticsForPREDA - getStatisticByName
##

setMethod("getStatisticByName", "StatisticsForPREDA", function(.Object, analysisName) {

analysesNames<-slot(.Object, "analysesNames")

    if (!(analysisName %in% analysesNames)) {
    stop("The selected analysisName is not available!")
    } else {
    col_id<-which(analysesNames==analysisName)
    statistics<-slot(.Object, "statistic")
    selected_statistic<-statistics[,col_id]
    return(selected_statistic)
    }

})


