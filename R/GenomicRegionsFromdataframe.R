#### 
#### AGGIUNGERE LA GESTIONE DELLE PTIONAL ANNOTATIONS
####




### 
### This is a function inizializing a GenomicAnnotations object from a generic dataframe
### 

GenomicRegionsFromdataframe<-function(GenomicRegions_dataframe, ids_column=NULL, chr_column, start_column, end_column, chromosomesNumbers=NULL, chromosomesLabels=NULL, chromosomesLabelsInput=NULL) {

# if chromosomes are provided as character strings, they should be translated into numeric chromosomes
if ((!is.null(chromosomesLabelsInput)) & (!is.null(chromosomesNumbers))) {
chromosome_translate_vector<-chromosomesNumbers
names(chromosome_translate_vector)<-as.character(chromosomesLabelsInput)
GenomicRegions_dataframe[,chr_column]<-as.numeric(chromosome_translate_vector[as.character(GenomicRegions_dataframe[,chr_column])])
}


# try to guess chr labels or numbers
if (is.null(chromosomesNumbers)) {
   if (is.null(chromosomesLabels)) {
   chromosomesNumbers<-unique(GenomicRegions_dataframe[,chr_column])
   chromosomesLabels<-as.character(chromosomesNumbers)
   } else {
   chromosomesNumbers<-(1:length(chromosomesLabels))
   }
}

# the ids of a GenomicRegions object can be NULL
if (is.null(ids_column)) {
output_ids<-NULL
} else {
output_ids<-as.character(GenomicRegions_dataframe[,ids_column])
}

GenomicRegions_data_object<-new("GenomicRegions",
 ids=output_ids,
 chr=as.numeric(GenomicRegions_dataframe[,chr_column]),
 start=as.numeric(GenomicRegions_dataframe[,start_column]),
 end=as.numeric(GenomicRegions_dataframe[,end_column]),
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels
)

return(GenomicRegions_data_object)

}


