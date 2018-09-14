

RMAwithCDFfilter<-function(SampleInfoFile=NULL, CELfiles_dir=NULL, AffyBatchInput=NULL,
custom_cdfname=NULL, custom_annotation=NULL, arrayNameColumn=NULL, sampleNameColumn=NULL, classColumn) {


# input provided either as AffyBatch object or list of cel files in an input file
if (!is.null(AffyBatchInput)) {
  if (!(inherits(AffyBatchInput, what="AffyBatch"))) {
  stop("the object AffyBatchInput is not an object of class AffyBatch")
  }
  AffyBatch_formatted_input<-TRUE
} else {
  if (is.null(SampleInfoFile)) {
  stop("Please provide input data either as a sampleinfo file or as an AffyBatch object")
  }
  if (is.null(CELfiles_dir)) {
  CELfiles_dir<-getwd()
  }
  AffyBatch_formatted_input<-FALSE
}


# SampleInfoFile<-"/home/francesco/Documents/PREDA/Esempio/SampleInfoFile.txt"
# CELfiles_dir<-"/home/francesco/Documents/PREDA/Esempio"
#  custom_cdfname<-"hugene10stv1hsentrezgcdf"
# arrayNameColumn=1
# sampleNameColumn=2
# classColumn=3

require(affy)
custom_cdfname<-cleancdfname(custom_cdfname)
if (is.null(custom_annotation)) {
custom_annotation<-gsub(pattern="cdf$", replacement=".db", perl=TRUE, x=custom_cdfname)
}


# verify if at least 4 probes per probeset are present in selcted custom CDF environment
require(custom_cdfname, character.only = TRUE)
cdfcustom<-as.list(get(custom_cdfname))
conteggio_probes<-sapply(cdfcustom, FUN=nrow)
selezione_probeset<-(conteggio_probes >= 4)
cdfcustom_filtered<-cdfcustom[selezione_probeset]


# remove Affymetrix control probesets (OPTIONAL STEP)
controls_selection<-(grep(pattern="^AFFX",x=names(cdfcustom_filtered), perl=TRUE))
    if( length(controls_selection) > 0) {
    cdfcustom_filtered<-cdfcustom_filtered[-controls_selection]
    }


# create the new CDF environment from filetered list
filtered_customcdf_env<-new.env()
multiassign(x=cdfcustom_filtered, envir =filtered_customcdf_env)
assign("filtered_customcdf_env", value=filtered_customcdf_env, envir=.GlobalEnv)


# if input is provided as affybatch object
if (AffyBatch_formatted_input) {
AffyBatchInput@cdfName<-"filtered_customcdf_env"
annotation(AffyBatchInput)<-custom_annotation

my_AnnotatedDataFrame<-new("AnnotatedDataFrame", data=data.frame("Class"=pData(AffyBatchInput)[,classColumn], row.names=sampleNames(AffyBatchInput)), varMetadata=data.frame(labelDescription="Samples classes", row.names="Class"))
phenoData(AffyBatchInput)<-my_AnnotatedDataFrame

my_ExpressionSet<-rma(AffyBatchInput)


} else {
# read sampleinfo file
sampleinfo<-read.table(SampleInfoFile, sep="\t", header=TRUE, stringsAsFactors=FALSE)

# RMA normalize data and obtain ExpressionSet object
# ATTENTI AL PARAMETRO cdfname !!
my_AnnotatedDataFrame<-new("AnnotatedDataFrame", data=data.frame("Class"=sampleinfo[,classColumn], row.names=sampleinfo[,sampleNameColumn]), varMetadata=data.frame(labelDescription="Samples classes", row.names="Class"))

my_ExpressionSet<-justRMA(filenames=sampleinfo[,arrayNameColumn], celfile.path=CELfiles_dir, sampleNames=sampleinfo[,sampleNameColumn], cdfname="filtered_customcdf_env", phenoData=my_AnnotatedDataFrame)
my_ExpressionSet@annotation<-custom_annotation
}

return(my_ExpressionSet)

}



