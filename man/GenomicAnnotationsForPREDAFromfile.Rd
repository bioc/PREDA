\name{GenomicAnnotationsForPREDAFromfile}
\alias{GenomicAnnotationsForPREDAFromfile}
\title{
Function to create a GenomicAnnotationsForPREDA object from a txt file
}
\description{
Function to create a GenomicAnnotationsForPREDA object from a txt file
}
\usage{
GenomicAnnotationsForPREDAFromfile(file, ids_column, chr_column,
start_column, end_column, strand_column, chromosomesNumbers =
NULL, chromosomesLabels = NULL, chromosomesLabelsInput = NULL,
MinusStrandString = "-", PlusStrandString = "+",
optionalAnnotationsColumns = NULL, reference_position_type =
"median", ...)
}
\arguments{
  \item{file}{
Path to the input txt file containing genomic annotations
}
  \item{ids_column}{
Specify the column from the input txt file with gene (or
other genomic features) ids. Can be specified using column
index (numeric) or column name (character).
}
  \item{chr_column}{
Specify the column from the input txt file with chromosome
annotations fields for each ids. Can be specified using column
index (numeric) or column name (character).
}
  \item{start_column}{
Specify the column from the input txt file with genomic start
position for each genomic element. Can be specified using column
index (numeric) or column name (character).
}
  \item{end_column}{
Specify the column from the input txt file with genomic end
position for each genomic element. Can be specified using column
index (numeric) or column name (character).
}
  \item{strand_column}{
Specify the column from the input txt file with genomic strand
mapping for each genomic element. Can be specified using
column index (numeric) or column name (character).
}
  \item{chromosomesNumbers}{
Numeric vector to specify the list of numeric values to be
associated to each chromosome (especially useful for chromosomes
not associated to a number such as chr X or Y)
}
  \item{chromosomesLabels}{
Character vector to specify the list of character labels to be
associated to each chromosome (especially useful for chromosomes
not associated to a number such as chr X or Y)
}
  \item{chromosomesLabelsInput}{
Character vector to specify the list of character labels 
associated to each chromosome in the input file. Particularly
useful when non numeric character strings are associated to
eacforh chromosome in the input file: e.g. "chr3" for chromosome
"3".
}
  \item{MinusStrandString}{
Character string used to identify minus strand in the input text
file
}
  \item{PlusStrandString}{
Character string used to identify plus strand in the input text
file
}
  \item{optionalAnnotationsColumns}{
Character vector of columns headers or numeric vector of columns
indices to specify columns of the input file containing
additional annotation fields
}
  \item{reference_position_type}{
Character string to specify which genomic coordinate must be
used as reference position for PREDA analysis. See also
\code{"\link{GenomicAnnotations2GenomicAnnotationsForPREDA}"}
}
  \item{\dots}{
any other parameter for read.table function that could be useful
for parsing the input file, such as "sep", "quote", "header",
"na.strings" and other parameters.
}
}
\value{
An object of class
\code{"\linkS4class{GenomicAnnotationsForPREDA}"}
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{GenomicAnnotationsForPREDA}"}
}
\examples{
  \dontrun{
 
data(PREDAsampledata)
 CNdataPath <- system.file("sampledata", "CopyNumber", package =
"PREDAsampledata")
CNannotationFile <- file.path(CNdataPath , "SNPAnnot100k.csv")

CNGenomicsAnnotations<-GenomicAnnotationsForPREDAFromfile(
  file=CNannotationFile,
  ids_column=1,
  chr_column="Chromosome",
  start_column=4,
  end_column=4,
  strand_column="Strand",
  chromosomesLabelsInput=1:22,
  MinusStrandString="-", PlusStrandString="+",
  optionalAnnotationsColumns=c("Cytoband", "Entrez_gene"),
  header=TRUE, sep=",", quote="\"", na.strings = c("NA", "",
  "---"))


  }
}
