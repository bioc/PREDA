\name{GenomicAnnotationsFilter_neg}
\alias{GenomicAnnotationsFilter_neg}
\alias{GenomicAnnotationsFilter_neg,DataForPREDA-method}
\alias{GenomicAnnotationsFilter_neg,GenomicAnnotations-method}
\alias{GenomicAnnotationsFilter_neg,GenomicAnnotationsForPREDA-method}
\title{
filter annotations to remove selected chromosomes
}
\description{
filter annotations to remove selected chromosomes
}
\usage{
# GenomicAnnotationsFilter_neg(.Object, chrToRemove, chrAsLabels=FALSE)

GenomicAnnotationsFilter_neg(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class GenomicAnnotations or classes inheriting from GenomicAnnotations
}
  \item{\dots}{
   See below
   \describe{
  \item{chrToRemove:}{
List of chromosomes to be removed from the annotations object.
}
  \item{chrAsLabels:}{
Logical, TRUE if chromosomes are listed as their character labels,
instead of using the numeric indexes
}
    }
  }
}
