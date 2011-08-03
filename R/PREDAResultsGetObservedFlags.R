
##
## method PREDAResultsGetObservedFlags : function for transforming the results of PREDA analyses into a matrix of status flags
##

##
## PREDAResults - PREDAResultsGetObservedFlags
##

setMethod("PREDAResultsGetObservedFlags", "PREDAResults", function(.Object, qval.threshold=0.05, smoothStatistic.tail=NULL, smoothStatistic.threshold=NULL, null.value=0, significant.value=1) {

    if (!is.null(smoothStatistic.threshold)) {
        if (is.null(smoothStatistic.tail)) {
        stop("Please specify the selected smooth statistic tail.")
        }
    }

    if (!is.null(smoothStatistic.tail)) {
        if (!(smoothStatistic.tail %in% c("upper", "lower"))) {
        stop("Smooth statistic tail can be only upper, lower or NULL.")
        } else if (is.null(smoothStatistic.threshold)) {
        stop("Please specify the selected smooth statistic threshold.")
        }
    }


# create skeleton for observed flags matrix output
ObservedFlags.matrix<-matrix(null.value, nrow=length(slot(.Object,"ids")), ncol=length(slot(.Object,"analysesNames")))

feature_selection<-(slot(.Object, "qvalue") <= qval.threshold)


    if (!is.null(smoothStatistic.threshold)) {
       if (smoothStatistic.tail=="upper") {
       feature_selection<-(feature_selection & (slot(.Object,"smoothStatistic")>= smoothStatistic.threshold))
       } else {
       feature_selection<-(feature_selection & (slot(.Object,"smoothStatistic")<= smoothStatistic.threshold))
       }
    }

ObservedFlags.matrix[which(feature_selection)]<-significant.value

colnames(ObservedFlags.matrix)<-slot(.Object, "analysesNames")
rownames(ObservedFlags.matrix)<-slot(.Object, "ids")


return(ObservedFlags.matrix)

})

