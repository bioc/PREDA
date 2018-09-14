\name{GenomicAnnotationsExtract}
\alias{GenomicAnnotationsExtract}
\alias{GenomicAnnotationsExtract,GenomicAnnotations-method}
\title{
extract optional annotations for a specific region
}
\description{
extract optional annotations for a specific region
}
\usage{
# GenomicAnnotationsExtract(.Object, chr, start, end,
# AnnotationsHeader=NULL, sep.character="; ",
# complete.inclusion=FALSE, skipSorting=FALSE,
# annotationAsRange=FALSE, getJustFeaturesNumber=FALSE)

GenomicAnnotationsExtract(.Object, ...)
}
\arguments{
  \item{.Object}{
    An object of class GenomicAnnotations
    }
  \item{\dots}{
   See below
   \describe{
    \item{chr:}{
    Coordinate for the selected genomic region
    }
    \item{start:}{
    Coordinate for the selected genomic region
    }
    \item{end:}{
    Coordinate for the selected genomic region
    }
    \item{AnnotationsHeader:}{
    Character or numeric vector to select the annotations columns
    to be considered
    }
    \item{sep.character:}{
    Character used to separate annotated features in the ouptput
    }
    \item{complete.inclusion:}{
    Logical, if TRUE only annotated features completely included
    in the region are reported. If FALSE (default), every overlapping 
    the feature is considered.
    }
    \item{skipSorting:}{
    Logical, if TRUE, annotation sorting is skipped before
    processing output (to save computational time, e.g. in a long loop)
    }
    \item{annotationAsRange:}{
    If TRUE, then only the first and last annotated element in
    the region are reported
    }
    \item{getJustFeaturesNumber:}{
    Logical: if TRUE, just the number of annotated features in
    the region is returned
    }
      }
    }
}
\details{
Extract annotations associated to a specific genomic region 
from a GenomiAnnotations object. Only annotations from the
specified columns are returned.
}
\value{
A character vector is returned
}
\seealso{
\code{"\linkS4class{GenomicAnnotations}"}
}


