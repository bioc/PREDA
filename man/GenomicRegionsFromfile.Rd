\name{GenomicRegionsFromfile}
\alias{GenomicRegionsFromfile}
\title{
Function to create a GenomiRegions object from a text file
}
\description{
Function to create a GenomiRegions object from a text file
}
\usage{
GenomicRegionsFromfile(file, ids_column=NULL, chr_column,
start_column, end_column, chromosomesNumbers=NULL,
chromosomesLabels=NULL, chromosomesLabelsInput=NULL, ...)
}
\arguments{
  \item{file}{
Path to the input txt file containing genomic regions annotations
}
  \item{ids_column}{
Specify the column from the input txt file with (optional) ids for genomic regions.
Can be specified using column index (numeric) or column name (character).
}
  \item{chr_column}{
Specify the column from the input txt file with chromosome
annotations fields. Can be specified using column
index (numeric) or column name (character).
}
  \item{start_column}{
Specify the column from the input txt file with genomic start
position for each genomic region. Can be specified using column
index (numeric) or column name (character).
}
  \item{end_column}{
Specify the column from the input txt file with genomic end
position for each genomic region. Can be specified using column
index (numeric) or column name (character).
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
each chromosome in the input file: e.g. "chr3" for chromosome
"3".
}
  \item{\dots}{
any other parameter for read.table function that could be useful
for parsing the input file, such as "sep", "quote", "header",
"na.strings" and other parameters.
}
}
\value{
An object of class \code{"\linkS4class{GenomicRegions}"}
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{GenomicRegions}"}
}
