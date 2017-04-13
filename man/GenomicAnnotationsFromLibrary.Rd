\name{GenomicAnnotationsFromLibrary}
\alias{GenomicAnnotationsFromLibrary}
\title{
Function extracting a GenomicAnnotations object from a
Bioconductor annotation library
}
\description{
Function extracting a GenomicAnnotations object from a
Bioconductor annotation library
}
\usage{
GenomicAnnotationsFromLibrary(annotLibrary, probeIDs = NULL,
retain.chrs = NULL, optionalAnnotations = NULL)
}
\arguments{
  \item{annotLibrary}{
Character string containing the name of the annotations library
to be used for building the GenomicAnnotations object
}
  \item{probeIDs}{
Optional: list of reference id from the selected annotLibrary to
be used for building the GenomicAnnotations object
}
  \item{retain.chrs}{
Numeric vector, containing the list of chromosomes selected for
the output GenomicAnnotations object. E.g. set retain.chrs=1:22
to limit the GenomicAnnotations object to chromosomes from 1 to
22. This might be ueseful to limit GenomiAnnotations objects to
autosomic chromosomes.
}
  \item{optionalAnnotations}{
Character vector to select additional annotations fields to be
included into the GenomicAnnotations object.
}
}
\value{
An object of class \code{"\linkS4class{GenomicAnnotations}"}
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{GenomicAnnotations}"}
}
\examples{
\dontrun{

GEGenomicAnnotations<-GenomicAnnotationsFromLibrary(annotLibrary=
"org.Hs.eg.db", retain.chrs=1:22)

# with optional annotations Genesymbols and EntrezGeneIDs
GEGenomicAnnotations<-GenomicAnnotationsFromLibrary(annotLibrary=
"hgu133plus2.db", retain.chrs=1:22,
optionalAnnotations=c("SYMBOL", "ENTREZID"))


}
}
