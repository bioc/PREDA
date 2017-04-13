\name{preprocessingGE}
\alias{preprocessingGE}
\title{
Wrapper function for gene expression data preprocessing for differential expression analysis with PREDA
}
\description{
Wrapper function for gene expression data preprocessing for differential expression analysis with PREDA
}
\usage{
preprocessingGE(SampleInfoFile = NULL, CELfiles_dir = NULL,
AffyBatchInput = NULL, custom_cdfname, arrayNameColumn = NULL,
sampleNameColumn = NULL, classColumn,
referenceGroupLabel, statisticType, optionalAnnotations = NULL,
retain.chrs = NULL, reference_position_type = "median",
testedTail = "both")
}
\arguments{
  \item{SampleInfoFile}{
Path to sample info file
}
  \item{CELfiles_dir}{
Path to directory containing raw CEL data files for Affymetrix
arrays
}
  \item{AffyBatchInput}{
Alternatively input raw data can be provided as an AffyBatch object.
In this case sample classes will be inferred from phenodata
contained in AffyBatch object. In particular classColumn parameter
will refer to the column in pData(AffyBatchInput) object.
}
  \item{custom_cdfname}{
Specify the cdf library to be used for data preprocessing
}
  \item{arrayNameColumn}{
Column of sampleinfo file containing the name of raw data
(CEL) files
}
  \item{sampleNameColumn}{
Column of sampleinfo file containing the name to be used for
samples labels
}
  \item{classColumn}{
Column of sampleinfo file containing the label of sample classes.
If input raw data are provided as an AffyBatch object,
this parameter refers intead to the column in pData(AffyBatchInput)
object.
}
  \item{referenceGroupLabel}{
Specify which class label is used for the reference sample used
in computing statistics for differential expression.
}
  \item{statisticType}{
Stastistic for differential expression that is computed on input
data. Possible values are "tstatistic", "SAM" (SAM statistical
score for differential expression), "FC" (Fold Change),
"FCmedian" (fold change computed on medians)
}
  \item{optionalAnnotations}{
Character vector to select additional annotations fields to be
included into the GenomicAnnotations object.
}
  \item{retain.chrs}{
Numeric vector, containing the list of chromosomes selected for
the output GenomicAnnotations object. E.g. set retain.chrs=1:22
to limit the GenomicAnnotations object to chromosomes from 1 to
22. This might be ueseful to limit GenomiAnnotations objects to
autosomic chromosomes.
}
  \item{reference_position_type}{
Specify which genomic coordinate must be used as reference
position for PREDA analysis. Possible values are "start", "end",
"median", "strand.start" or "strand.end".
}
  \item{testedTail}{
  Specify what tail of the distribution will be tested for
significantly extreme values in PREDA analysis. Possible values
are "both", "upper" or "lower".
}
}
\details{
Preprocess raw (CEL) files for Affymetrix gene expression
arrays using user defined CDF libraries and RMA normalization.
Then statistics for differential expression are computed.
Then annotations are retrieved from the corresponding
annotation library.

Please note this function is a user-friendly preprocessing
function for Affy gene expression microarrays.
Step by step preprocessing functions can be used
with any other platform.

}
\value{
A DataForPREDA object is returned.
}
\author{
Francesco Ferrari
}
\seealso{
\code{\linkS4class{DataForPREDA}}
}
\examples{

\dontrun{

require("PREDAsampledata")
CELfilesPath <- system.file("sampledata", "GeneExpression",
package = "PREDAsampledata")
infofile <- file.path(CELfilesPath , "sampleinfoGE_PREDA.txt")
sampleinfo<-read.table(infofile, sep="\t", header=TRUE)


GEDataForPREDA<-preprocessingGE(SampleInfoFile=infofile,
CELfiles_dir=CELfilesPath,
custom_cdfname="hgu133plus2",
arrayNameColumn=1,
sampleNameColumn=2,
classColumn="Class",
referenceGroupLabel="normal",
statisticType="tstatistic",
optionalAnnotations=c("SYMBOL", "ENTREZID"),
retain.chrs=1:22
)
}

}
