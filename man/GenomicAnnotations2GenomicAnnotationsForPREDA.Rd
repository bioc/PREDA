\name{GenomicAnnotations2GenomicAnnotationsForPREDA}
\alias{GenomicAnnotations2GenomicAnnotationsForPREDA}
\alias{GenomicAnnotations2GenomicAnnotationsForPREDA,GenomicAnnotations-method}
\title{
generate a GenomicAnnotationsForPREDA object from a GenomicAnnotations object
}
\description{
generate a new GenomicAnnotationsForPREDA object from a GenomicAnnotations object
}
\usage{
# GenomicAnnotations2GenomicAnnotationsForPREDA(.Object,
# positions=NULL, reference_position_type=NULL)

GenomicAnnotations2GenomicAnnotationsForPREDA(.Object,
... )
}
\arguments{
  \item{.Object}{
An object of class GenomicAnnotations
}
  \item{\dots}{
   See below
   \describe{
  \item{positions:}{
Vector to specify reference positions for
GenomicAnnotationsForPREDA object if not specified with
reference_position_type parameter
}
  \item{reference_position_type:}{
Specify which genomic coordinate must be used as reference
position for PREDA analysis. Possible values are "start", "end",
"median", "strand.start" or "strand.end".

"strand.start" is strand specific start: i.e. start on positive strand
but end on negative strand. "strand.end" is strand specific end.
}
  }
}
}
\value{
A GenomicAnnotationsForPREDA object
}
\author{
Francesco Ferrari
}
\seealso{
\code{\linkS4class{GenomicAnnotationsForPREDA}}
}
\examples{
 \dontrun{
 
GEGenomicAnnotations<-GenomicAnnotationsFromLibrary(annotLibrary
= "org.Hs.eg.db", retain.chrs=1:22)

  GEGenomicAnnotationsForPREDA<-
  GenomicAnnotations2GenomicAnnotationsForPREDA(
  GEGenomicAnnotations, reference_position_type="median")

  }
}
