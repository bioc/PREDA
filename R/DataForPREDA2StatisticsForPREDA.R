
##
## method DataForPREDA2StatisticsForPREDA is used to extract the StatisticsForPREDA object from the DataForPREDA object
##

##
## DataForPREDA - DataForPREDA2StatisticsForPREDA
##

setMethod("DataForPREDA2StatisticsForPREDA", "DataForPREDA", function(.Object) {

StatisticsForPREDA_object<-new("StatisticsForPREDA", ids=slot(.Object,"ids"), statistic=slot(.Object,"statistic"), analysesNames=slot(.Object,"analysesNames"), testedTail=slot(.Object,"testedTail"))

return(StatisticsForPREDA_object)

})
