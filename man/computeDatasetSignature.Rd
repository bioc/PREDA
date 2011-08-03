\name{computeDatasetSignature}
\alias{computeDatasetSignature}
\alias{computeDatasetSignature,GenomicAnnotationsForPREDA-method}
\title{
Function to compute dataset signature for recurrent significant genomic regions
}
\description{
Function to compute dataset signature for recurrent significant genomic regions
}
\usage{
# computeDatasetSignature(.Object, genomicRegionsList=genomicRegionsList,
# multTestCorrection="fdr", signature_qval_threshold=0.05,
# returnRegions=TRUE, use.referencePositions=TRUE)

computeDatasetSignature(.Object, ...)
}
\arguments{
  \item{.Object}{
Object of class GenomicAnnotationsForPREDA
}
  \item{\dots}{
  See below
   \describe{
      \item{genomicRegionsList:}{
    List of genomicRegions objects for which the recurrent
    overlapping regions will be evaluated
    }
      \item{multTestCorrection:}{
    Multiple testing correction that will be adopted to correct the
    statistic p-values. Possible values are "fdr", for benjamini and
    Hochberg multiple testing correction and "qvalue" for p-values
    correction performed with qvalue package.
    }
      \item{signature_qval_threshold:}{
    Threshold used to select significant regions resulting from the
    dataset signature statistic
    }
      \item{returnRegions:}{
    Logical, if TRUE (default) the genomic regions constituting the
    daaset signature are returned, otherwise a PREDAresults object
    containing dataset signature statistics is returned.
    }
      \item{use.referencePositions:}{
    Logical, if TRUE the input reference positions used for PREDA
    analysis wil be used to identify significant genomic regions
    boundaries as well.
      }
  }
}

}
\details{
The function adopts a binomial test to identify significant
recurrence of genomic regions across multiple dataset sampels.
}
\value{
A GenomicRegions object (if returnRegions = TRUE) or
a PREDAresults object containing dataset signature statistics (if
returnRegions = FALSE)
}
\author{
Francesco Ferrari
}
\seealso{
\code{\linkS4class{GenomicRegions}},
\code{\linkS4class{PREDAResults}}
}
\examples{
 \dontrun{

  require(PREDAsampledata)
  data(SODEGIRCNanalysisResults)
  data(GEDataForPREDA)

  SODEGIR_CN_GAIN<-PREDAResults2GenomicRegions(
  SODEGIRCNanalysisResults, qval.threshold=0.01,
  smoothStatistic.tail="upper", smoothStatistic.threshold=0.1)


  CNgain_signature<-computeDatasetSignature(GEDataForPREDA,
  genomicRegionsList=SODEGIR_CN_GAIN)

  }
}

