

### 
### This is a function reading a txt file, with custom format
### 



### The definition of the optional parameter "na.strings" should be indeed strongly recommended by the function manuals in order to avoid mistakes...

GenomicAnnotationsFromfile<-function(file, ids_column, chr_column, start_column, end_column, strand_column, chromosomesNumbers=NULL, chromosomesLabels=NULL, chromosomesLabelsInput=NULL, MinusStrandString="-", PlusStrandString="+", optionalAnnotationsColumns=NULL, ...) {

GenomicAnnotations_data<-read.table(file=file, stringsAsFactors=FALSE, ...)

GenomicAnnotations_data_object<-GenomicAnnotationsFromdataframe(GenomicAnnotations_dataframe=GenomicAnnotations_data, ids_column=ids_column, chr_column=chr_column, start_column=start_column, end_column=end_column, strand_column=strand_column, chromosomesNumbers=chromosomesNumbers, chromosomesLabels=chromosomesLabels, chromosomesLabelsInput=chromosomesLabelsInput, MinusStrandString=MinusStrandString, PlusStrandString=PlusStrandString, optionalAnnotationsColumns=optionalAnnotationsColumns)

return(GenomicAnnotations_data_object)

}




