
##
## method StatisticsForPREDAFilterColumns_pos to filter analyses to retain selected analyses
##

##
## StatisticsForPREDA - StatisticsForPREDAFilterColumns_pos
##

setMethod("StatisticsForPREDAFilterColumns_pos", "StatisticsForPREDA", function(.Object, analysesToRetain, analysesAsNames=FALSE) {

# extracting analyses List and labels
selected_analysesNames_list<-slot(.Object, "analysesNames")
if (analysesAsNames) {
mySelection<-(!(selected_analysesNames_list %in% analysesToRetain))
analysesToRemove<-selected_analysesNames_list[mySelection]
} else {
selected_analyses_list<-(1:length(selected_analysesNames_list))
mySelection<-(!(selected_analyses_list %in% analysesToRetain))
analysesToRemove<-selected_analyses_list[mySelection]
}


# applying negative filtering
StatisticsForPREDA_object<-StatisticsForPREDAFilterColumns_neg(.Object , analysesToRemove=analysesToRemove, analysesAsNames=analysesAsNames)

return(StatisticsForPREDA_object)


})


##
## DataForPREDA - StatisticsForPREDAFilterColumns_pos
##

setMethod("StatisticsForPREDAFilterColumns_pos", "DataForPREDA", function(.Object, analysesToRetain, analysesAsNames=FALSE) {

# extracting analyses List and labels
selected_analysesNames_list<-slot(.Object, "analysesNames")
if (analysesAsNames) {
mySelection<-(!(selected_analysesNames_list %in% analysesToRetain))
analysesToRemove<-selected_analysesNames_list[mySelection]
} else {
selected_analyses_list<-(1:length(selected_analysesNames_list))
mySelection<-(!(selected_analyses_list %in% analysesToRetain))
analysesToRemove<-selected_analyses_list[mySelection]
}


# applying negative filtering
DataForPREDA_object<-StatisticsForPREDAFilterColumns_neg(.Object , analysesToRemove=analysesToRemove, analysesAsNames=analysesAsNames)

return(DataForPREDA_object)


})

