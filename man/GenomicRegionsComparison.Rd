\name{GenomicRegionsComparison}
\alias{GenomicRegionsComparison}
\alias{GenomicRegionsComparison,GenomicRegions,GenomicRegions-method}
\title{
compare GenomicRegions objects to identify overlaps and differences
}
\description{
compare GenomicRegions objects to identify overlaps and differences
}
\usage{
GenomicRegionsComparison(.Object1, .Object2)
}
\arguments{
  \item{.Object1}{
  An object of Class GenomicRegions
}
  \item{.Object2}{
  An object of Class GenomicRegions
}
}
\details{
 Compare GenomicRegions objects to identify overlaps and differences
}
\value{

   A list containing:
 \item{overlapping.regions}{GenomicRegions object describing
    the overlapping regions between input object1 and object2}
 \item{difference.1.2}{GenomicRegions object describing the
    regions from input object1 not overlapping regions from object2}
 \item{difference.2.1}{GenomicRegions object describing the
    regions from input object2 not overlapping regions from object1}
 \item{GenomicRegions1.number}{Number of genomic regions in input object1}
 \item{GenomicRegions2.number}{Number of genomic regions in input object2}
 \item{overlapping.number}{Number of overlapping genomic
    regions between input object1 and object2}
 \item{GenomicRegions1.totalspan}{Total span of genomic regions in input object1}
 \item{GenomicRegions2.totalspan}{Total span of genomic regions in input object2}
 \item{overlapping.totalspan}{Total span of overlapping genomic
    regions between input object1 and object2}
 \item{overlap.VS.GenomicRegions1.ratio}{Ratio between overlapping
    regions and regions from input object1}
 \item{overlap.VS.GenomicRegions2.ratio}{Ratio between overlapping
    regions and regions from input object2}
 
}
\author{
  Francesco Ferrari
}
\seealso{
  \code{\link{GenomicRegionsFindOverlap}},
  \code{\linkS4class{GenomicRegions}}
}
