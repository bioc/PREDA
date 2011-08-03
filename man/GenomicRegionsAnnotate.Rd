\name{GenomicRegionsAnnotate}
\alias{GenomicRegionsAnnotate}
\alias{GenomicRegionsAnnotate,GenomicRegions,GenomicAnnotations-method}
\title{
extract annotations from a GenomicAnnotations object for a set of regions specified as a GenomicRegions object
}
\description{
extract annotations from a GenomicAnnotations object for a set of regions specified as a GenomicRegions object
}
\usage{
# GenomicRegionsAnnotate(.Object1, .Object2,
# AnnotationsHeaders=NULL, sep.character="; ",
# complete.inclusion=FALSE, annotationAsRange=FALSE,
# getJustFeaturesNumber=FALSE)

GenomicRegionsAnnotate(.Object1, .Object2, ...)
}
\arguments{
  \item{.Object1}{
  An object of class GenomicRegions
  }
  \item{.Object2}{
  An object of class GenomicAnnotations
  }
  \item{\dots}{
   See below
   \describe{
    \item{AnnotationsHeaders:}{
    Names of optional annotations fields from GenomicAnnotations 
    object that are used to annotate the GenomicRegions object.
    Multiple annotation fields can be used
    }
    \item{sep.character:}{
    Character sequence used to separate annotation features
    }
    \item{complete.inclusion:}{
    Logical, if TRUE only annotations features entirely covered by 
    one of the genomic regions are considered. (e.g. a gene
    completely included in the genomic regions from start to end)
    If FALSE also partial overlapping annotation features are used
    }
    \item{annotationAsRange:}{
    Logical, if TRUE only the first and last annotation features
    associated to each the genomic region are returned
    }
    \item{getJustFeaturesNumber:}{
    Logical, if TRUE only the numbers of annotation features
    overlapping the genomic regions are returned.
    If TRUE, only the first element specified with
    AnnotationsHeaders parameter is considered.
    }
   }
  }
}
\details{
The annotation features overlapping the input genomic regions
are used to add optional annotations field to the GenomicRegions
object.

If previous optional annotations fields are present,
they are preserved as well in the output object
}
\value{
A GenomicRegions object with optionalAnnotations 
}
