\name{GenomicRegions-class}
\Rdversion{1.1}
\docType{class}
\alias{GenomicRegions-class}
\title{Class "GenomicRegions" is used to manage information about genomic regions }
\description{This class is used to manage genomic regions information that can be derived from PREDA analysis results
or from other sources:e.g. relevant genomic regions from literature reports can be imported into a GenomicRegions object and compared with PREDA analysis results}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GenomicRegions", chr, start, end, chromosomesNumbers, chromosomesLabels, optionalAnnotations, optionalAnnotationsHeaders, ids)}.
}
\section{Slots}{
  \describe{
    \item{\code{chr}:}{Object of class \code{"integer"} a numeric vector representing the chromosome where each genomic region is located.
       Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
       e.g. for Human, chromsomomes X and Y will be converted to chromsomes 23 and 24 respectively.
       User defined options will allow this conversion during GenomicAnnotations objects initialization. }
    \item{\code{start}:}{Object of class \code{"integer"} a numeric vector of start genomic position for each genomic region. This vector must have the same length of "chr" slot. }
    \item{\code{end}:}{Object of class \code{"integer"} a numeric vector of end genomic position for each genomic region. This vector must have the same length of "chr" slot. }
    \item{\code{chromosomesNumbers}:}{Object of class \code{"numeric"} a numeric vector containing the list of chromosomes associated to genomic regions in the GenomicRegions object.
                      Each chromosome is represented just once in increasing order. Please note that chromosomes usually not represented with a number will be comverted to a number as well.
                      e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively. }
    \item{\code{chromosomesLabels}:}{Object of class \code{"character"} a character vector containing the list of chromosomes associated to genomic regions in the GenomicRegions object.
                     Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
                     This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
                     (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human) }
    \item{\code{optionalAnnotations}:}{Object of class \code{"matrix"} optional annotations associated to the genomic regions can be managed along with GenomicRegions objects.
                       E.g. the list of GeneSymbol or EntrezGene ids associated to each genomic region can be provided as optional annotation.
                       These additional annotations are not mandatory (the default value for this slot is NULL)
                       The additional annotations must be provided as a matrix of character,
                       with a number of rows equal to the length of "chr", "start" and "end" slots and a number of columns equal
                       to le thength of "optionalAnnotationsHeaders" slot. }
    \item{\code{optionalAnnotationsHeaders}:}{Object of class \code{"character"} the list of names associated to optional annotations. Please avoid using spaces in annotations names. }
    \item{\code{ids}:}{Object of class \code{"character"} a character vector of unique identifiers associated to each genomic regions. This is just an optional element of GenomicRegions objects: the default value is NULL. }
  }
}
\section{Methods}{
  \describe{
    \item{GenomicRegions2dataframe}{\code{signature(.Object = "GenomicRegions")}: extract genomic regions information as a dataframe object }
    \item{GenomicRegionsAnnotate}{\code{signature(.Object1 = "GenomicRegions", .Object2 = "GenomicAnnotations")}: extract annotations from a GenomicAnnotations object for a set of regions specified as a GenomicRegions object }
    \item{GenomicRegionsChrNumber}{\code{signature(.Object = "GenomicRegions")}: determine the number of chromosomes with genomic regions }
    \item{GenomicRegionsComparison}{\code{signature(.Object1 = "GenomicRegions", .Object2 = "GenomicRegions")}: compare GenomicRegions objects to identify overlaps }
    \item{GenomicRegionsCreateRegionsIds}{\code{signature(.Object = "GenomicRegions")}: generate unique ids for GenomicRegions objects }
    \item{GenomicRegionsFilter_neg}{\code{signature(.Object = "GenomicRegions")}: filter genomic regions to remove selected chromosomes }
    \item{GenomicRegionsFilter_pos}{\code{signature(.Object = "GenomicRegions")}: filter genomic regions to keep selected chromosomes }
    \item{GenomicRegionsNumber}{\code{signature(.Object = "GenomicRegions")}: determine the number of genomic regions }
    \item{GenomicRegionsSpan}{\code{signature(.Object = "GenomicRegions")}: determine the span of each genomic region }
    \item{GenomicRegionsTotalSpan}{\code{signature(.Object = "GenomicRegions")}: determine the total span of genomic regions }
    \item{initialize}{\code{signature(.Object = "GenomicRegions")}: initialize method for GenomicRegions objects }
	 }
}
\author{ Francesco Ferrari }
\note{ This class is better described in the package vignette}
\seealso{
\code{\link{GenomicAnnotationsSortAndCleanNA}},\code{\link{PREDADataAndResults2dataframe}}
}
\examples{
showClass("GenomicRegions")
}
\keyword{classes}
