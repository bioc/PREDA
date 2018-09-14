\name{GenomicRegionsFilter_pos}
\alias{GenomicRegionsFilter_pos}
\alias{GenomicRegionsFilter_pos,GenomicRegions-method}
\title{
filter genomic regions to keep selected chromosomes
}
\description{
filter genomic regions to keep selected chromosomes
}
\usage{
# GenomicRegionsFilter_pos(.Object, chrToRetain, chrAsLabels=FALSE, quiet=FALSE)

GenomicRegionsFilter_pos(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class GenomicRegions
}
  \item{\dots}{
   See below
   \describe{
  \item{chrToRetain:}{
List of chromosomes to be maintained after removing the genomic regions for all the other chromosomes.
}
  \item{chrAsLabels:}{
Logical, TRUE if chromosomes are listed as their character labels,
instead of using the numeric indexes
}
  \item{quiet:}{
Logical, if FALSE a message is printed to warn of empty (NULL) result of the filtering selection.
}
    }
  }
}
