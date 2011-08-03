\name{MergeStatisticAnnotations2DataForPREDA}
\alias{MergeStatisticAnnotations2DataForPREDA}
\title{
Merge a StatisticsForPREDA and a GenomicAnnotationsForPREDA object into a DataForPREDA object.
}
\description{
This function merges a StatisticsForPREDA and a GenomicAnnotationsForPREDA object into a DataForPREDA object
}
\usage{
MergeStatisticAnnotations2DataForPREDA(StatisticsForPREDAObject,
GenomicAnnotationsForPREDAObject, sortAndCleanNA = FALSE, quiet =
FALSE, MedianCenter = FALSE)
}
\arguments{
  \item{StatisticsForPREDAObject}{
An object of class StatisticsForPREDA
}
  \item{GenomicAnnotationsForPREDAObject}{
An object of class GenomicAnnotationsForPREDA
}
  \item{sortAndCleanNA}{
Logical, if TRUE, genomic annotations are sorted for chromosome
and genomic position then ids with NA positinal annotations are
removed
}
  \item{quiet}{
Logical, if TRUE messages reporting the number of unmatched
ids are suppressed.
}
  \item{MedianCenter}{
Logical, if TRUE data are normalized per median sample.
}
}
\value{
An object of class DataForPREDA
}
\author{
Francesco Ferrari
}
