\name{GenomicAnnotationsSortAndCleanNA}
\alias{GenomicAnnotationsSortAndCleanNA}
\alias{GenomicAnnotationsSortAndCleanNA,DataForPREDA-method}
\alias{GenomicAnnotationsSortAndCleanNA,PREDAResults-method}
\alias{GenomicAnnotationsSortAndCleanNA,GenomicAnnotations-method}
\alias{GenomicAnnotationsSortAndCleanNA,PREDADataAndResults-method}
\alias{GenomicAnnotationsSortAndCleanNA,GenomicAnnotationsForPREDA-method}
\title{
sort annotations according to selected chromosomes and to remove genes containing any NA annotation field
}
\description{
sort annotations according to selected chromosomes and to remove genes containing any NA annotation field
}
\usage{
# GenomicAnnotationsSortAndCleanNA(.Object, sorting_position_column="start")

GenomicAnnotationsSortAndCleanNA(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class GenomicAnnotations or any object inheriting from GenomicAnnotations
}
  \item{\dots}{
   See below
   \describe{
  \item{sorting_position_column:}{
Annotations slot used to sort data within each chromosome.
Possilbe values include "start", "end" or "position" (the last one for GenomicAnnotationsForPREDA objects)
}
    }
  }

}
