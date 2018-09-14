### 
### This is a function reading a txt file, with custom format and creating a StatisticsForPREDA object from its content
### 


### The definition of the optional parameter "na.strings" should be indeed strongly recommended by the function manuals in order to avoid mistakes...

StatisticsForPREDAFromfile<-function(file, ids_column=NULL, statistic_columns=NULL, analysesNames=NULL, testedTail=c("upper", "lower","both"), ...) {

StatisticsForPREDA_data<-read.table(file=file, stringsAsFactors=FALSE, ...)

StatisticsForPREDA_data_object<-StatisticsForPREDAFromdataframe(StatisticsForPREDA_dataframe=StatisticsForPREDA_data, ids_column=ids_column, statistic_columns=statistic_columns, analysesNames=analysesNames, testedTail=testedTail)

return(StatisticsForPREDA_data_object)


}

