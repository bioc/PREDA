

### 
### This is a function inizializing a GenomicAnnotations object from a generic dataframe
### 

GenomicAnnotationsFromdataframe<-function(GenomicAnnotations_dataframe, ids_column, chr_column, start_column, end_column, strand_column, chromosomesNumbers=NULL, chromosomesLabels=NULL, chromosomesLabelsInput=NULL, MinusStrandString="-", PlusStrandString="+", optionalAnnotationsColumns=NULL) {

if (!is.null(chromosomesLabelsInput)) {
chromosomesLabelsInput<-as.character(chromosomesLabelsInput)
}
if (!is.null(chromosomesLabels)) {
chromosomesLabels<-as.character(chromosomesLabels)
}



# try to guess chr labels or numbers
if (is.null(chromosomesNumbers)) {
   if (is.null(chromosomesLabels)) {
        if (is.null(chromosomesLabelsInput)) {
        stop("Please specify the chromosomesLabels, or chromosomesLabelsInput or chromosomesNumbers")
        } else {
        chromosomesLabels<-chromosomesLabelsInput
        chromosomesNumbers<-(1:length(chromosomesLabels))
        }
   } else {
   chromosomesNumbers<-(1:length(chromosomesLabels))
   }
} else {
   if (is.null(chromosomesLabels)) {
        if (is.null(chromosomesLabelsInput)) {
        stop("Please specify the chromosomesLabels or chromosomesLabelsInput")
        } else {
        chromosomesLabels<-chromosomesLabelsInput
        }
   }
}


# if chromosomes are provided as character strings, they should be translated into numeric chromosomes
if ((!is.null(chromosomesLabelsInput)) & (!is.null(chromosomesNumbers))) {
chromosome_translate_vector<-chromosomesNumbers
names(chromosome_translate_vector)<-as.character(chromosomesLabelsInput)
GenomicAnnotations_dataframe[,chr_column]<-as.numeric(chromosome_translate_vector[as.character(GenomicAnnotations_dataframe[,chr_column])])
}



# # character strings defining the genes strand should be replaced with -1 or 1 for minus and plus strand respectively
# set to NA the unmatching strand strings
selection_NAs<-(!(GenomicAnnotations_dataframe[,strand_column] %in% c(MinusStrandString, PlusStrandString)))
if (any(selection_NAs)) {
GenomicAnnotations_dataframe[which(selection_NAs),strand_column]<-NA
}

# set to the right numeric value the minus and plus strand strings
selection_Minus<-(GenomicAnnotations_dataframe[,strand_column] == MinusStrandString)
if (any(selection_Minus)) {
GenomicAnnotations_dataframe[which(selection_Minus),strand_column]<-(-1)
}

selection_Plus<-(GenomicAnnotations_dataframe[,strand_column] == PlusStrandString)
if (any(selection_Plus)) {
GenomicAnnotations_dataframe[which(selection_Plus),strand_column]<-(1)
}

GenomicAnnotations_dataframe[,strand_column]<-as.numeric(GenomicAnnotations_dataframe[,strand_column])

if (is.null(optionalAnnotationsColumns)) {
optionalAnnotations<-NULL
optionalAnnotationsHeaders<-NULL
} else if ((is.character(optionalAnnotationsColumns) & any(!(optionalAnnotationsColumns %in% colnames(GenomicAnnotations_dataframe)))) | (is.numeric(optionalAnnotationsColumns)  & any(!(optionalAnnotationsColumns %in% 1:ncol(GenomicAnnotations_dataframe))))) {
stop("The selected optionalAnnotationsColumns are not available")
} else if (length(optionalAnnotationsColumns)>1) {
optionalAnnotationsHeaders<-colnames(GenomicAnnotations_dataframe[,optionalAnnotationsColumns])
optionalAnnotations<-(GenomicAnnotations_dataframe[,optionalAnnotationsColumns])
  # we have to force the conversion from numeric to character column by column due to a possible "bug" of R in converting numeric columns to character
  for (colindex in 1:ncol(optionalAnnotations)) {
  optionalAnnotations[,colindex]<-as.character(optionalAnnotations[,colindex])
  }
optionalAnnotations<-as.matrix(optionalAnnotations)
} else {
    if (is.numeric(optionalAnnotationsColumns)) {
    optionalAnnotationsHeaders<-colnames(GenomicAnnotations_dataframe)[optionalAnnotationsColumns]
    } else {
    optionalAnnotationsHeaders<-optionalAnnotationsColumns
    }
optionalAnnotations<-as.matrix(GenomicAnnotations_dataframe[,optionalAnnotationsHeaders])
# we have to force the conversion from numeric to character column by column due to a possible "bug" of R in converting numeric columns to character
mode(optionalAnnotations)<-"character"
}


GenomicAnnotations_data_object<-new("GenomicAnnotations",
 ids=as.character(GenomicAnnotations_dataframe[,ids_column]),
 chr=as.numeric(GenomicAnnotations_dataframe[,chr_column]),
 start=as.numeric(GenomicAnnotations_dataframe[,start_column]),
 end=as.numeric(GenomicAnnotations_dataframe[,end_column]),
 strand=as.numeric(GenomicAnnotations_dataframe[,strand_column]),
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders
)

return(GenomicAnnotations_data_object)

}


