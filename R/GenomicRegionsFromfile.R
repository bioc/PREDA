

GenomicRegionsFromfile<-function(file, ids_column=NULL, chr_column, start_column, end_column, chromosomesNumbers=NULL, chromosomesLabels=NULL, chromosomesLabelsInput=NULL, ...) {


GenomicRegions_data<-read.table(file=file, stringsAsFactors=FALSE, ...)

GenomicRegions_data_object<-GenomicRegionsFromdataframe(GenomicRegions_dataframe=GenomicRegions_data, ids_column=ids_column, chr_column=chr_column, start_column=start_column, end_column=end_column, chromosomesNumbers=chromosomesNumbers, chromosomesLabels=chromosomesLabels, chromosomesLabelsInput=chromosomesLabelsInput)

return(GenomicRegions_data_object)

}


