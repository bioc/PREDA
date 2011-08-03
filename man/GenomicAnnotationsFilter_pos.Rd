\name{GenomicAnnotationsFilter_pos}
\alias{GenomicAnnotationsFilter_pos}
\alias{GenomicAnnotationsFilter_pos,DataForPREDA-method}
\alias{GenomicAnnotationsFilter_pos,GenomicAnnotations-method}
\alias{GenomicAnnotationsFilter_pos,GenomicAnnotationsForPREDA-method}
\title{
filter annotations to keep selected chromosomes
}
\description{
filter annotations to keep selected chromosomes
}
\usage{
# GenomicAnnotationsFilter_pos(.Object, chrToRetain, chrAsLabels=FALSE)

GenomicAnnotationsFilter_pos(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class GenomicAnnotations or classes inheriting from GenomicAnnotations
}
  \item{\dots}{
   See below
   \describe{
  \item{chrToRetain:}{
List of chromosomes to be maintained after removing the annotations for all the other chromosomes.
}
  \item{chrAsLabels:}{
Logical, TRUE if chromosomes are listed as their character labels,
instead of using the numeric indexes
}
    }
  }
}
