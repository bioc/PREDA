
##
## method analysesNames to retrieve analysesNames
##

##
## StatisticsForPREDA - analysesNames
##

setMethod("analysesNames", "StatisticsForPREDA", function(.Object) {

analysesNames<-slot(.Object, "analysesNames")

return(analysesNames)

})


##
## StatisticsForPREDA - PREDAResults
##

setMethod("analysesNames", "PREDAResults", function(.Object) {

analysesNames<-slot(.Object, "analysesNames")

return(analysesNames)

})

