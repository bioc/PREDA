##
## method compareFunctionFromStatisticsForPREDA to retrieve the function for comparing observed and permuted smooth statistic
##

##
## StatisticsForPREDA - compareFunctionFromStatisticsForPREDA
##

setMethod("compareFunctionFromStatisticsForPREDA", "StatisticsForPREDA", function(.Object) {

selected_Tail<-slot(.Object,"testedTail")

if (selected_Tail=="both") {

    compareWithObservedSmoothFunction<-function(observed, permuted) {
    count_extremes<-ifelse(abs(observed)>=abs(permuted), yes=1, no=0)
    return(count_extremes)
    }

} else if (selected_Tail=="upper") {

    compareWithObservedSmoothFunction<-function(observed, permuted) {
    count_extremes<-ifelse((observed>=permuted), yes=1, no=0)
    return(count_extremes)
    }

} else if (selected_Tail=="lower") {

    compareWithObservedSmoothFunction<-function(observed, permuted) {
    count_extremes<-ifelse((observed<=permuted), yes=1, no=0)
    return(count_extremes)
    }

}

return(compareWithObservedSmoothFunction)


})

