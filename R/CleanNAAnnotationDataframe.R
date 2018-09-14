

### 
### Function removing genes from annotation dataframe with NA in any of the selected annotation fields
### 

CleanNAAnnotationDataframe<-function(input_dataframe, selected_columns_list=c("chr", "start", "end", "strand")) {

any_na_annotations<-apply(input_dataframe[,selected_columns_list],1,FUN=function(x) {
return(any(is.na(x)))
})
input_dataframe<-input_dataframe[!(any_na_annotations),]

return(input_dataframe)

}


