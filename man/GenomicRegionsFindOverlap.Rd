\name{GenomicRegionsFindOverlap}
\alias{GenomicRegionsFindOverlap}
\title{
Function to find overlap between GenomicRegions objects
}
\description{
Function to find overlap between GenomicRegions objects
}
\usage{
GenomicRegionsFindOverlap(GenomicRegions1, GenomicRegions2 = NULL)
}
\arguments{
  \item{GenomicRegions1}{
Either a GenomicRegions object or a list of GenomicRegions
objects
}
  \item{GenomicRegions2}{
Optiona with default value NULL. Either a GenomicRegions object
or a list of GenomicRegions objects.
}
}
\details{
Input genomic regions object are compared to select overlapping
genomic regions that are returned as GenomicRegions objects.

If two single GenomicRegions object are provided, just one
comparison is performed and one single GenomicRegions object is
returned.

If one single list of GenomicRegions objects is provided as
input, then the included GenomicRegions objects are compared to
select overlapping GenomicRegions across all of the elements.

If two lists of GenomicRegions objects are provided as
input, they must have the same number of elements, because
element by element comparison will be performed to
identify overlapping GenomicRegions across all of the elements.

}
\value{
Either a single GenomicRegions objec or a list of GenomicRegions
objecs.
}
\author{
Francesco Ferrari
}
\seealso{
\code{\link{GenomicRegionsComparison}},
\code{\linkS4class{GenomicRegions}}
}
\examples{
\dontrun{
require(PREDAsampledata)
data(SODEGIRCNanalysisResults)
data(SODEGIRGEanalysisResults)

SODEGIR_GE_UP<-PREDAResults2GenomicRegions(
SODEGIRGEanalysisResults, qval.threshold=0.05,
smoothStatistic.tail="upper", smoothStatistic.threshold=0.5)

SODEGIR_CN_GAIN<-PREDAResults2GenomicRegions(
SODEGIRCNanalysisResults, qval.threshold=0.01,
smoothStatistic.tail="upper", smoothStatistic.threshold=0.1)

SODEGIR_AMPLIFIED<-GenomicRegionsFindOverlap(SODEGIR_GE_UP,
SODEGIR_CN_GAIN)

}
}

