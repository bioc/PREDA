\name{GenomicAnnotations2reference_positions}
\alias{GenomicAnnotations2reference_positions}
\alias{GenomicAnnotations2reference_positions,GenomicAnnotations-method}
\title{
extract reference positions from the GenomicAnnotations
}
\description{
extract from the GenomicAnnotations object a vector containing a vector with reference positions
}
\usage{
# GenomicAnnotations2reference_positions(.Object,
# reference_position_type=c("start", "end", "median", "strand.start", "strand.end"),
# withnames=TRUE)

GenomicAnnotations2reference_positions(.Object, ...)
}
\arguments{
  \item{.Object}{
Object of class GenomicAnnotations
}
  \item{\dots}{
   See below
   \describe{
  \item{reference_position_type:}{
Specify which genomic coordinate must be used as reference
position for PREDA analysis. Possible values are "start", "end",
"median", "strand.start" or "strand.end".

"strand.start" is strand specific start: i.e. start on positive strand
but end on negative strand. "strand.end" is strand specific end.
}
  \item{withnames:}{
Logical, if TRUE the "ids" slot content is used as names for the output vector
}
  }
}
}
\value{
A numeric vector with the selected reference positions.
}

