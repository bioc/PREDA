\name{GenomicAnnotations-class}
\Rdversion{1.1}
\docType{class}
\alias{GenomicAnnotations-class}
\title{Class "GenomicAnnotations" to manage information about genomic features }
\description{This class is used to manage information about genomic features under investigation: i.e. genomic genes, SNP or others, with particular focus on the genomic coordinates of each of them.
Other additional annotations associated to each element can be stored in a GenomicAnnotations object in the optionalAnnotations slots}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GenomicAnnotations",
ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels,
optionalAnnotations, optionalAnnotationsHeaders)}.
}
\section{Slots}{
  \describe{
    \item{\code{ids}:}{Object of class \code{"character"} ~~ }
    \item{\code{chr}:}{Object of class \code{"integer"} ~~ }
    \item{\code{start}:}{Object of class \code{"integer"} ~~ }
    \item{\code{end}:}{Object of class \code{"integer"} ~~ }
    \item{\code{strand}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{chromosomesNumbers}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{chromosomesLabels}:}{Object of class \code{"character"} ~~ }
    \item{\code{optionalAnnotations}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{optionalAnnotationsHeaders}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{GenomicAnnotations2dataframe}{\code{signature(.Object = "GenomicAnnotations")}: extracts annotations as a dataframe with probeids as rownames }
    \item{GenomicAnnotations2GenomicAnnotationsForPREDA}{\code{signature(.Object = "GenomicAnnotations")}: generate a new GenomicAnnotationsForPREDA object from a GenomicAnnotations object }
    \item{GenomicAnnotations2reference_positions}{\code{signature(.Object = "GenomicAnnotations")}: extract from the GenomicAnnotations object a vector containing a vector with reference positions }
    \item{GenomicAnnotationsExtract}{\code{signature(.Object = "GenomicAnnotations")}: extract optional annotations for a specific region }
    \item{GenomicAnnotationsFilter_neg}{\code{signature(.Object = "GenomicAnnotations")}: filter annotations to remove selected chromosomes }
    \item{GenomicAnnotationsFilter_pos}{\code{signature(.Object = "GenomicAnnotations")}: filter annotations to keep selected chromosomes }
    \item{GenomicAnnotationsSortAndCleanNA}{\code{signature(.Object = "GenomicAnnotations")}: sort annotations according to selected chromosomes and to remove genes containing any NA annotation field }
    \item{GenomicRegionsAnnotate}{\code{signature(.Object1 = "GenomicRegions", .Object2 = "GenomicAnnotations")}: extract annotations from a GenomicAnnotations object for a set of regions specified as a GenomicRegions object }
    \item{initialize}{\code{signature(.Object = "GenomicAnnotations")}: initialize method for GenomicAnnotations objects }
	 }
}
\author{ Francesco Ferrari }
\note{ This class is better described in the package vignette}
\seealso{
    \code{\link{GenomicAnnotations2dataframe}}, \code{\link{GenomicAnnotations2GenomicAnnotationsForPREDA}}, 
    \code{\link{GenomicAnnotations2reference_positions}},\code{\link{GenomicAnnotationsExtract}},
    \code{\link{GenomicAnnotationsFilter_neg}},\code{\link{GenomicAnnotationsFilter_pos}},
    \code{\link{GenomicAnnotationsSortAndCleanNA}},\code{\link{GenomicRegionsAnnotate}},
}
\examples{
showClass("GenomicAnnotations")
}
\keyword{classes}
