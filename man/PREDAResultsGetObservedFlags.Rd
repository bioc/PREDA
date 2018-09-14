\name{PREDAResultsGetObservedFlags}
\alias{PREDAResultsGetObservedFlags}
\alias{PREDAResultsGetObservedFlags,PREDAResults-method}
\title{
extract genomic positions with significant alterations as a matrix of flags from a PREDAResults object
}
\description{
extract genomic positions with significant alterations as a matrix of flags from a PREDAResults object
}
\usage{
# PREDAResultsGetObservedFlags(.Object, qval.threshold=0.05,
# smoothStatistic.tail=NULL, smoothStatistic.threshold=NULL,
# null.value=0, significant.value=1)

PREDAResultsGetObservedFlags(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class PREDAResults or PREDADataAndResults
}
  \item{\dots}{
   See below
   \describe{
  \item{qval.threshold:}{
    q-value threshold used to identify significant genomic positions
      }
      \item{smoothStatistic.tail:}{
    Possible values are "upper" or "lower". This parameter specify
    if only one tail of the smoothed statististic distribution must
    be considered. If it is NULL, both tails are used and
    smoothStatistic.threshold is ignored.
      }
      \item{smoothStatistic.threshold:}{
    Threshold on smoothStatistic values to select significant
    genomic regions.
      }
      \item{null.value:}{
    Value (flag) assigned to not significant positions
    }
      \item{significant.value:}{
    Value (flag) assigned to significant positions
    }
  }
}
}
