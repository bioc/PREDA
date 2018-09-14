
GenomicAnnotationsFromLibrary<-function(annotLibrary, probeIDs=NULL, retain.chrs=NULL, optionalAnnotations=NULL) {

require(annotate)
annotLibrary<-annPkgName(annotLibrary)
require(annotLibrary, character.only = TRUE)

    if (is.null(probeIDs)) {
    probesets<-keys(get(paste(gsub(pattern="\\.db$", replacement="", perl=TRUE, x=annotLibrary), "CHR", sep="")))
    } else {
    probesets<-probeIDs
    }



my_collapse<-function(x) {paste(x, collapse="; ")}
my_collapse_min<-function(x) {min(abs(x))}
my_collapse_max<-function(x) {max(abs(x))}
my_collapse_sign<-function(x) {my_collapse(unique(sign(x)))}


Chr<-suppressWarnings(sapply(lookUp(probesets, data=annotLibrary, what="CHR"), my_collapse))
Start<-suppressWarnings(abs(as.numeric(sapply(lookUp(probesets, data=annotLibrary, what="CHRLOC"), my_collapse))))
End<-suppressWarnings(abs(as.numeric(sapply(lookUp(probesets, data=annotLibrary, what="CHRLOCEND"), my_collapse))))

## %%% this is a patch because the current on line version of GeneAnnot custom CDF annotation packages do not include the strand sign in CHRLOC and CHRLOCEND slots
if (grepl(pattern="^gahgu", perl=TRUE, x=annotLibrary)) {
Strand<-suppressWarnings(as.character((sapply(lookUp(probesets, data=annotLibrary, what="GENESTRAND"), my_collapse))))
Strand[which(Strand=="Plus")]<-"1"
Strand[which(Strand=="Minus")]<-"-1"
} else {
Strand<-suppressWarnings(as.character(sign(as.numeric(sapply(lookUp(probesets, data=annotLibrary, what="CHRLOC"), my_collapse)))))
}
    
# GeneSymbols<-sapply(lookUp(probesets, data=my_ExpressionSet@annotation, what="SYMBOL"), my_collapse)
# Entrez<-sapply(lookUp(probesets, data=my_ExpressionSet@annotation, what="ENTREZID"), my_collapse)

#my_Annotations<-data.frame("probeset_id"=probesets, "GeneSymbols"=GeneSymbols,  "Entrez"=Entrez, "Chr"=Chr, "Start"=Start, "End"=End, "Strand"=Strand, stringsAsFactors=FALSE)
my_Annotations<-data.frame("probeset_id"=probesets, "Chr"=Chr, "Start"=Start, "End"=End, "Strand"=Strand, stringsAsFactors=FALSE)

    if (!is.null(optionalAnnotations)) {
    optionalAnnotations_matrix<-NULL
        for (optionalAnnotation_column in optionalAnnotations) {
        current_optionalAnnotation<-sapply(lookUp(probesets, data=annotLibrary, what=optionalAnnotation_column), my_collapse)
        optionalAnnotations_matrix<-cbind(optionalAnnotations_matrix, current_optionalAnnotation)
        }
    colnames(optionalAnnotations_matrix)<-optionalAnnotations
    my_Annotations<-data.frame(my_Annotations, optionalAnnotations_matrix, stringsAsFactors=FALSE)
    }



#### save output files
current_chr_labels<-sort(suppressWarnings(as.numeric(unique(Chr))), na.last=NA)
current_chr_labels_nonnumeric<-unique(Chr)[is.na(suppressWarnings(as.numeric(unique(Chr))))]
current_chr_labels_nonnumeric_selectionOrder<-NULL
current_chr_labels_nonnumeric_selectionOrder<- c(current_chr_labels_nonnumeric_selectionOrder, grep("^X$", perl=TRUE, x=current_chr_labels_nonnumeric, ignore.case = TRUE))
current_chr_labels_nonnumeric_selectionOrder<- c(current_chr_labels_nonnumeric_selectionOrder, grep("^Y$", perl=TRUE,x=current_chr_labels_nonnumeric, ignore.case = TRUE))
current_chr_labels_nonnumeric_selectionOrder<- c(current_chr_labels_nonnumeric_selectionOrder, c(1:length(current_chr_labels_nonnumeric))[-(current_chr_labels_nonnumeric_selectionOrder)])
current_chr_labels<-c(current_chr_labels, current_chr_labels_nonnumeric[current_chr_labels_nonnumeric_selectionOrder])




GenomicsAnnotations_object<-GenomicAnnotationsFromdataframe(
GenomicAnnotations_dataframe=my_Annotations,
ids_column="probeset_id",
chr_column="Chr",
start_column="Start",
end_column="End",
strand_column="Strand",
MinusStrandString="-1",
PlusStrandString="1",
chromosomesNumbers=c(1:length(unique(current_chr_labels))),
chromosomesLabels=current_chr_labels,
chromosomesLabelsInput=current_chr_labels,
optionalAnnotationsColumns=optionalAnnotations
)

# select the chromosomes retained for the analysis
    if (is.null(retain.chrs)) {
    retain.chrs<-slot(GenomicsAnnotations_object, name="chromosomesNumbers")
    }

GenomicsAnnotations_object<-GenomicAnnotationsFilter_pos(GenomicsAnnotations_object, chrToRetain=retain.chrs)

return(GenomicsAnnotations_object)

}

