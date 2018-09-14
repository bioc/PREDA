\name{GenomicRegionsFilter_neg}
\alias{GenomicRegionsFilter_neg}
\alias{GenomicRegionsFilter_neg,GenomicRegions-method}
\title{
filter genomic regions to remove selected chromosomes
}
\description{
filter genomic regions to remove selected chromosomes
}
\usage{
# GenomicRegionsFilter_neg(.Object, chrToRemove, chrAsLabels=FALSE, quiet=FALSE)

GenomicRegionsFilter_neg(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class GenomicRegions
}
  \item{\dots}{
   See below
   \describe{
  \item{chrToRemove:}{
List of chromosomes to be removed from the genomic regions object.
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
