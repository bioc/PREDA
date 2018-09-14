\name{GenomicRegionsFromdataframe}
\alias{GenomicRegionsFromdataframe}
\title{
Function to create a GenomiRegions object from a dataframe
}
\description{
Function to create a GenomiRegions object from a dataframe
}
\usage{
GenomicRegionsFromdataframe(GenomicRegions_dataframe, ids_column=NULL, chr_column,
start_column, end_column, chromosomesNumbers=NULL,
chromosomesLabels=NULL, chromosomesLabelsInput=NULL)
}
\arguments{
  \item{GenomicRegions_dataframe}{
Dataframe object containing the annotations for genomic regions
}
  \item{ids_column}{
Specify the column from the input dataframe with (optional) ids for genomic regions.
Can be specified using column index (numeric) or column name (character).
}
  \item{chr_column}{
Specify the column from the input dataframe with chromosome
annotations fields. Can be specified using column
index (numeric) or column name (character).
}
  \item{start_column}{
Specify the column from the input dataframe with genomic start
position for each genomic region. Can be specified using column
index (numeric) or column name (character).
}
  \item{end_column}{
Specify the column from the input dataframe with genomic end
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
associated to each chromosome in the input. Particularly
useful when non numeric character strings are associated to
each chromosome in the input file: e.g. "chr3" for chromosome
"3".
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
