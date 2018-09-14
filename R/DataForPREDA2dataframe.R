##
## Method DataForPREDA2dataframe is used to extract the DataForPREDA object information as a dataframe
##

##
## DataForPREDA - DataForPREDA2dataframe
##

setMethod("DataForPREDA2dataframe", "DataForPREDA", function(.Object) {

output_dataframe_part1<-GenomicAnnotationsForPREDA2dataframe(.Object)
output_dataframe_part2<-StatisticsForPREDA2dataframe(.Object)

output_dataframe<-data.frame(output_dataframe_part1, output_dataframe_part2[,(-(which(colnames(output_dataframe_part2)=="ids")))], stringsAsFactors=FALSE)
rownames(output_dataframe)<-output_dataframe[,"ids"]
return(output_dataframe)

})

