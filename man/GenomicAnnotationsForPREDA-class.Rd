\name{GenomicAnnotationsForPREDA-class}
\Rdversion{1.1}
\docType{class}
\alias{GenomicAnnotationsForPREDA-class}
\title{Class "GenomicAnnotationsForPREDA" GenomicAnnotations class with additional slot specifying the reference position for PREDA analysis}
\description{This class is equivalent to the GenomicAnnotations class but includes an additional slot specifying 
the reference position that will be used for PREDA smoothing of data: this is included in the "position" slot.
An unique reference position is required for PREDA analysis because this position is used for smoothing data along chromosomal coordinates.
This reference position usaually is the start, the end, or the median posizion of each considered genomic feature, nevertheless other user defined positions could be used as well.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GenomicAnnotationsForPREDA", ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, position, optionalAnnotations, optionalAnnotationsHeaders)}.
}
\section{Slots}{
  \describe{
    \item{\code{position}:}{Object of class \code{"integer"} a numeric vector of reference genomic positions that will be associated and used for each genomic feature under investigation for smoothing data during PREDA analysis. }
    \item{\code{ids}:}{Object of class \code{"character"} a character vector of unique identifiers for the genomic features under investigation }
    \item{\code{chr}:}{Object of class \code{"integer"} a numeric vector representing the chromosome where each ids is mapped. 
            Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
            e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
            User defined options will allow this conversion during GenomicAnnotations objects initialization. }
    \item{\code{start}:}{Object of class \code{"integer"} a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements). }
    \item{\code{end}:}{Object of class \code{"integer"} a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements). }
    \item{\code{strand}:}{Object of class \code{"numeric"} a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
          User defined options will allow the conversion to this format during GenomicAnnotations objects initialization. }
    \item{\code{chromosomesNumbers}:}{Object of class \code{"numeric"} a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
                      Each chromosome is represented just once in increasing order. Please note that chromosome usually not represented with a number must will be comverted to a number as well.
                      e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively. }
    \item{\code{chromosomesLabels}:}{Object of class \code{"character"} a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
                     Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
                     This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
                     (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human) }
    \item{\code{optionalAnnotations}:}{Object of class \code{"matrix"} optional annotations associated to the genomic features can be managed along with genomic positions annotations.
                       E.g. GeneSymbol or EntrezGene ids can be associated to gene realted GenomicAnnotaitons objects.
                       These additional annotations are not mandatory (the default value for this slot is NULL)
                       The additional annotations must be provided as a matrix of character,
                       with a number of rows equal to the length of "ids" slot and a number of columns equal
                       to the length of "optionalAnnotationsHeaders" slot. }
    \item{\code{optionalAnnotationsHeaders}:}{Object of class \code{"character"} character vector containing the names associated to optional annotations. Please avoid using spaces in annotations names. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{GenomicAnnotations}"}, directly.
}
\section{Methods}{
  \describe{
    \item{genomePlot}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: draw a genome plot  }
    \item{GenomicAnnotations2dataframe}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}:  extract annotations as a dataframe with probeids as rownames }
    \item{GenomicAnnotationsFilter_neg}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: filter annotations to remove selected chromosomes }
    \item{GenomicAnnotationsFilter_pos}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: filter annotations to keep selected chromosomes }
    \item{GenomicAnnotationsForPREDA2dataframe}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: extract annotations as a dataframe with probeids as rownames }
    \item{GenomicAnnotationsForPREDA2GenomicAnnotations}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: extract the GenomicAnnotations object from the GenomicAnnotationsForPREDA object }
    \item{GenomicAnnotationsForPREDA2PREDAResults}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: add PREDA results information to genomic annotatations creating a PREDAResults object }
    \item{GenomicAnnotationsSortAndCleanNA}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: sort annotations according to selected chromosomes and to remove genes containing any NA annotation field }
    \item{initialize}{\code{signature(.Object = "GenomicAnnotationsForPREDA")}: initialize method for GenomicAnnotationsForPREDA objects }
	 }
}
\author{ Francesco Ferrari }
\note{ This class is better described in the package vignette}
\seealso{
\code{"\linkS4class{GenomicAnnotations}"}, \code{\link{GenomicAnnotationsSortAndCleanNA}},
\code{\link{GenomicAnnotationsForPREDA2PREDAResults}},\code{\link{GenomicAnnotationsForPREDA2GenomicAnnotations}},
\code{\link{GenomicAnnotationsForPREDA2dataframe}},\code{\link{GenomicAnnotationsFilter_pos}},
\code{\link{GenomicAnnotationsFilter_neg}},\code{\link{GenomicAnnotations2dataframe}},\code{\link{genomePlot}}
}
\examples{
showClass("GenomicAnnotationsForPREDA")
}
\keyword{classes}
