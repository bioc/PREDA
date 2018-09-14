\name{SODEGIR_GEstatistics}
\alias{SODEGIR_GEstatistics}
\alias{SODEGIR_GEstatistics,ExpressionSet-method}
\title{
Wrapper function for gene expression statistics preprocessing for SODEGIR analysis
}
\description{
Wrapper function for gene expression statistics preprocessing for SODEGIR analysis.
}
\usage{
# SODEGIR_GEstatistics(.Object, pData_classColumn=NULL,
# referenceGroupLabel=NULL,
# statisticType=c("tstatistic", "FC", "FCmedian", "eBayes"),
# singleSampleOutput=TRUE, varianceAll=FALSE)

SODEGIR_GEstatistics(.Object, ...)
}
\arguments{
  \item{.Object}{
  An object of class ExpressionSet containing gene expression input data
  }
  \item{\dots}{
   See below
   \describe{
      \item{pData_classColumn:}{
      Column of phenoData slot from the ExpressionSet object, containing the label of sample classes
      }
      \item{referenceGroupLabel:}{
      Specify which class label is used for the reference sample used
      in computing statistics for differential expression.
      }
      \item{statisticType:}{
      Stastistic for differential expression that is computed on input
      data. Possible values are "tstatistic", "FC" (Fold Change),
      "FCmedian" (fold change computed on medians)
      }
      \item{singleSampleOutput:}{
      Logical, if TRUE a statistic comparing each sample with the
      reference group is computed.
      }
      \item{varianceAll:}{
      This parameter affect the computation only when
      singleSampleOutput is TRUE.
      
      varianceAll is itself a logical parameter. If TRUE,
      all pathological (e.g. tumor) samples
      and all normal (reference) samples are used to estimate 
      variance in the comparison of individual pathological samples 
      to the normal reference, as described in the
      original SODEGIR apper by Bicciato et al. (Nucleic Acids Res. 2009).

      The original SODEGIR statistic for Gene Expression was based on
      the SAM score. However, since July 2018 the samr package is no more available in CRAN.
      Therefore in the current PREDA version the
      varianceAll=TRUE parameter can't be used as SAM is not available.
      When singleSampleOutput is TRUE and a different statisticType
      is used, the variance is actually computed using only the normal
      (reference) samples.

      If FALSE (default value), the computation of statistics for single sample VS
      reference comparisons only take into account the variance in the
      reference group of samples.
      }
    }
  }

}
\details{
Using an ExpressionSet object as input,
statistics for differential expression are computed
comparing each sample with the reference group.
}
\value{
The output is returned as a matrix.
}
\references{
Silvio Bicciato, Roberta Spinelli, Mattia Zampieri, Eleonora
Mangano, Francesco Ferrari, Luca Beltrame, Ingrid Cifola, Clelia
Peano, Aldo Solari, and Cristina Battaglia.
A computational procedure to identify significant overlap
of differentially expressed and genomic imbalanced regions in
cancer datasets. Nucleic Acids Res, 37(15):5057-70, August 2009.
}
\author{
Francesco Ferrari
}
\seealso{
  \code{\link{preprocessingGE}},
  \code{\link{SODEGIRpreprocessingGE}},
  \code{\linkS4class{ExpressionSet}}
}
