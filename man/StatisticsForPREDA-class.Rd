\name{StatisticsForPREDA-class}
\Rdversion{1.1}
\docType{class}
\alias{StatisticsForPREDA-class}
\title{Class "StatisticsForPREDA" is used to manage the datamatrix containing statistics for PREDA analyses }
\description{This class is used to manage the datamatrix containing statistics for PREDA analyses: i.e. the gene (or other genomic feature) centered statistics accounting for differential expression
(or for the other type of variation under investigation)}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("StatisticsForPREDA", ids, statistic, analysesNames, testedTail)}.
}
\section{Slots}{
  \describe{
    \item{\code{ids}:}{Object of class \code{"character"} a character vector of unique identifiers for the genomic features under investigation }
    \item{\code{statistic}:}{Object of class \code{"matrix"} a numeric matrix containing gene-centered statistics (or statistics on genomic data centered on other genomic features under investigation).
             The statistics must be provided as a matrix of numeric values,
             with a number of rows equal to the length of "ids" slot and a number of columns equal
             to the length of "analysesNames" slot. }
    \item{\code{analysesNames}:}{Object of class \code{"character"} a character vector of unique names associated to each column of statistic matrix.
                 This is just a name that will be used to identify each analysis. }
    \item{\code{testedTail}:}{Object of class \code{"character"} a character describing what tail of the statistic distribution will be analyzed during PREDA analysis.
              Possible values are "upper", "lower" or "both". Anyway we strongly recommend using PREDA analysis only
              for statistics on genomic data with a symmetric distribution around zero. }
  }
}
\section{Methods}{
  \describe{
    \item{analysesNames}{\code{signature(.Object = "StatisticsForPREDA")}: get the names of the analyses in the StatisticsForPREDA object }
    \item{getStatisticByName}{\code{signature(.Object = "StatisticsForPREDA")}: extract data for individual analyses using the analysis name }
    \item{initialize}{\code{signature(.Object = "StatisticsForPREDA")}: initialize method for StatisticsForPREDA objects }
    \item{StatisticsForPREDA2dataframe}{\code{signature(.Object = "StatisticsForPREDA")}: extract data as a dataframe with probeids as rownames }
    \item{StatisticsForPREDAFilterColumns_neg}{\code{signature(.Object = "StatisticsForPREDA")}: filter statistics to remove selected analyses }
    \item{StatisticsForPREDAFilterColumns_pos}{\code{signature(.Object = "StatisticsForPREDA")}: filter statistics to keep selected analyses }
	 }
}
\author{ Francesco Ferrari }
\note{ This class is better described in the package vignette}
\seealso{
 \code{"\linkS4class{DataForPREDA}"}, 
\code{\link{analysesNames}},\code{\link{getStatisticByName}}
\code{\link{StatisticsForPREDA2dataframe}},
\code{\link{StatisticsForPREDAFilterColumns_neg}},\code{\link{StatisticsForPREDAFilterColumns_pos}}
}
\examples{
showClass("StatisticsForPREDA")
}
\keyword{classes}
