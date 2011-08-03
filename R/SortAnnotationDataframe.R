###
### This is a function to sort a dataframe containing genomic annotations
###

SortAnnotationDataframe<-function(input_dataframe, selected_chromosomes_list, chromosomes_column, sorting_position_column) {

selected_chromosomes_list<-sort(selected_chromosomes_list)
data_sorted<-NULL
  for (cc in selected_chromosomes_list) {
  cc_subset_selection<-(input_dataframe[,chromosomes_column]==cc)
  cc_subset_positions<-input_dataframe[cc_subset_selection, sorting_position_column]
  cc_subset_data<-input_dataframe[cc_subset_selection,]
    #ordering data on selected column
    ordine<-order(cc_subset_positions)
    #saving output into data_sorted
    data_sorted<-rbind(data_sorted, cc_subset_data[ordine,])
  }
return(data_sorted)

}
