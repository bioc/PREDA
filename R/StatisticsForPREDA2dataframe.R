##
## Method StatisticsForPREDA2dataframe to extract annotations as a dataframe with probeids as rownames
##

##
## StatisticsForPREDA - StatisticsForPREDA2dataframe
##

setMethod("StatisticsForPREDA2dataframe", "StatisticsForPREDA", function(.Object) {
output_dataframe<-data.frame("ids"=slot(.Object, "ids"), slot(.Object, "statistic"), stringsAsFactors=FALSE)
colnames(output_dataframe)<-c("ids", slot(.Object, "analysesNames"))
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)
})


