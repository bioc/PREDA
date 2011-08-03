
### 
### This is a function inizializing a StatisticsForPREDA object from a generic dataframe
### 


StatisticsForPREDAFromdataframe<-function(StatisticsForPREDA_dataframe, ids_column=NULL, statistic_columns=NULL, analysesNames=NULL, testedTail=c("upper", "lower","both") ) {

  if (length(ids_column)==1) {
    if (is.numeric(ids_column)) {
       if (is.null(statistic_columns)) {statistic_columns<-c(1:ncol(StatisticsForPREDA_dataframe))[-(ids_column)]}
    } else if (is.character(ids_column)) {
       if (is.null(statistic_columns)) {statistic_columns<-c(1:ncol(StatisticsForPREDA_dataframe))[-(which(colnames(StatisticsForPREDA_dataframe)==ids_column))] }
    }
  out_ids<-as.character(StatisticsForPREDA_dataframe[,ids_column])

  } else if (length(ids_column)==nrow(StatisticsForPREDA_dataframe)) {
  out_ids<-as.character(ids_column)
    if (is.null(statistic_columns)) {statistic_columns<-c(1:ncol(StatisticsForPREDA_dataframe))}
  } else {
  stop("The ids_column parameter should contain either a vector of ids (one for each row) or a single column name/id specifying the dataframe column containing the ids.")
  }


  out_statistic<-as.matrix(StatisticsForPREDA_dataframe[,statistic_columns])
  mode(out_statistic)<-"numeric"


  if (is.null(analysesNames)) {
  analysesNames<-colnames(StatisticsForPREDA_dataframe[,statistic_columns,drop=FALSE])
  }

  if (length(analysesNames) != length(statistic_columns)) {
  stop("The number of analysis names is different from the number of statistic columns.")
  }


  StatisticsForPREDA_data_object<-new("StatisticsForPREDA",
  ids=out_ids,
  statistic=out_statistic,
  analysesNames=analysesNames,
  testedTail=testedTail
  )

  return(StatisticsForPREDA_data_object)

}




