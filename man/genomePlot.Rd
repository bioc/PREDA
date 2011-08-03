\name{genomePlot}
\alias{genomePlot}
\alias{genomePlot,GenomicAnnotationsForPREDA-method}
\title{
draw a genome plot
}
\description{
draw a genome plot with user defined genomic regions
}
\usage{
# genomePlot(.Object, genomicRegions=NULL, draw.blocks=TRUE,
# parallel.plot=TRUE, grouping=NULL, custom.labels=NULL,
#  scale.positions=NULL, qval.threshold=0.05,
# use.referencePositions=FALSE, smoothStatistic.tail=NULL,
# smoothStatistic.threshold=NULL, region.colors=NULL,
# limitChrs=NULL)

genomePlot(.Object, ...)
}
\arguments{
  \item{.Object}{
Object of class GenomicAnnotationsForPREDA, or any other class
exteinding this one.
}
  \item{\dots}{
  See below
   \describe{
    
      \item{genomicRegions:}{
    A list of GenomicRegions object containing the genomic regions
    to be highlighted in the plot.
    }
      \item{draw.blocks:}{
    If TRUE genomic regions are plotted as blocks. Otherwise they
    are plotted as coloured ticks. Currently only draw.blocks=TRUE is
    implemented.
    }
      \item{parallel.plot:}{
    Logical, if TRUE multiple copies of each chrosomosome are drawn.

    In particular a number of copies equal to lnegth(grouping), if
    grouping is not null, or a number of copies equal to the number
    of GenomicRegions objects provided as input.
    }
      \item{grouping:}{
    Vector specifying how input GenomicRegions objects will be
    grouped on chromosomes.
    }
    \item{custom.labels:}{
    A character to specify user defined labels for vertical axis
    }
    \item{scale.positions:}{
    Parameter to set the scale for chromosomal positions (horizontal
    axis). Possible values are "Mb" or "Kb"
    }
    \item{qval.threshold:}{
    If genomicRegions is NULL, and a PREDAResults or
    PREDADataAndResults is provided as input, the function
    PREDAResults2GenomcRegions is applied with this parameter to
    extract significant GenomicRegions.
    }
    \item{use.referencePositions:}{
    If genomicRegions is NULL, and a PREDAResults or
    PREDADataAndResults is provided as input, the function
    PREDAResults2GenomcRegions is applied with this parameter to
    extract significant GenomicRegions.
    }
    \item{smoothStatistic.tail:}{
    If genomicRegions is NULL, and a PREDAResults or
    PREDADataAndResults is provided as input, the function
    PREDAResults2GenomcRegions is applied with this parameter to
    extract significant GenomicRegions.
    }
    \item{smoothStatistic.threshold:}{
    If genomicRegions is NULL, and a PREDAResults or
    PREDADataAndResults is provided as input, the function
    PREDAResults2GenomcRegions is applied with this parameter to
    extract significant GenomicRegions.
    }
    \item{region.colors:}{
    Character vector specifyin the list of colors to be used for
    drawing each set of GenomicRegions. Mut be of length equal to
    the number of GenomicRegions objects provided as input.
    }
    \item{limitChrs:}{
    Numeric vector, that can be used to limit the plot to a subset
    of chromosomes.
    }
   }
  }
}
\details{
See also the PREDA tutorial vignette for more details and sample
usage
}
\value{
A plot of the genome with significant GenomicRegions
}
\author{
Francesco Ferrari
}
\seealso{
\code{\link{PREDAResults2GenomicRegions}},
\code{\linkS4class{PREDAResults}},  
\code{\linkS4class{PREDADataAndResults}},  
\code{\linkS4class{GenomicAnnotationsForPREDA}}
}
\examples{
## See PREDA tutorial vignette for some examples

}

