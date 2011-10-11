\name{GenomicRegions2dataframe}
\alias{GenomicRegions2dataframe}
\title{
extract genomic regions information as a dataframe object
}
\description{
extract genomic regions information as a dataframe object
}
\usage{
GenomicRegions2dataframe(GenomicRegionsObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GenomicRegionsObject}{
Object of class genomic regions
}
}
\details{
Extract genomic regions information as a dataframe object
}
\value{
A dataframe object
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

 
dataframe_UPregions<-GenomicRegions2dataframe(
genomic_regions_UP[[1]])
  }

}
