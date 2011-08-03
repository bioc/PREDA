\name{PREDAResults2GenomicRegionsSingle}
\alias{PREDAResults2GenomicRegionsSingle}
\alias{PREDAResults2GenomicRegionsSingle,PREDAResults-method}
\title{
identify significant genomic regions from a single analysis in a PREDAResults object
}
\description{
identify significant genomic regions from a single analysis in a PREDAResults object
}
\usage{
# PREDAResults2GenomicRegionsSingle(.Object,
# qval.threshold=0.05, analysisName=NULL,
# use.referencePositions=TRUE, smoothStatistic.tail=NULL,
# smoothStatistic.threshold=NULL)

PREDAResults2GenomicRegionsSingle(.Object, ...)
}
\arguments{
  \item{.Object}{
  Object of class PREDAResults or PREDADataAndResults
  }
  \item{\dots}{
   See below
   \describe{
      \item{qval.threshold:}{
    q-value threshold used to identify significant genomic regions
      }
      \item{analysisName:}{
    name of the analysis to be considered
      }
      \item{use.referencePositions:}{
    Logical, if TRUE the input reference positions used for PREDA
    analysis wil be used to identify significant genomic regions
    boundaries as well.
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
  }
}
}
