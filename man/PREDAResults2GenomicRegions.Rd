\name{PREDAResults2GenomicRegions}
\alias{PREDAResults2GenomicRegions}
\alias{PREDAResults2GenomicRegions,PREDAResults-method}
\title{
identify significant genomic regions from a PREDAResults object
}
\description{
identify significant genomic regions from a PREDAResults object
}
\usage{
# PREDAResults2GenomicRegions(.Object, qval.threshold=0.05,
# use.referencePositions=TRUE, smoothStatistic.tail=NULL,
# smoothStatistic.threshold=NULL)

PREDAResults2GenomicRegions(.Object, ...)
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
\details{
A list og genomic regions objects is returned: one
GenomicRegions object for each analysis in the input
PREDAresults.

A NULL element is included in the output list whenever no
siginifcant regions are identified.
}
\value{
A list of genomic regions objects 
}
\author{
Francesco Ferrari
}
\examples{
\dontrun{
require(PREDAsampledata)

data(GEanalysisResults)

genomic_regions_UP<-PREDAResults2GenomicRegions(GEanalysisResults
, qval.threshold=0.05, smoothStatistic.tail="upper",
smoothStatistic.threshold=0.5)

}
}

