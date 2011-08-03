\name{statisticsForPREDAfromEset}
\alias{statisticsForPREDAfromEset}
\alias{statisticsForPREDAfromEset,ExpressionSet-method}
\title{
function to compute a statisticsForPREDA object from an ExpressionSet object
}
\description{
function to compute a statisticsForPREDA object from an ExpressionSet object
}
\usage{
# statisticsForPREDAfromEset(.Object, pData_classColumn=NULL,
# statisticType=NULL, logged=TRUE, referenceGroupLabel=NULL,
# classVector=NULL, testedTail="both")

statisticsForPREDAfromEset(.Object, ...)
}
\arguments{
    \item{.Object}{
  Object of class ExpressionSet
  }
  \item{\dots}{
   See below
   \describe{
      \item{pData_classColumn:}{
    Column from pData(.Object) containig the labels for different
    samples classes.
    }
      \item{statisticType:}{
  Stastistic for differential expression that is computed on input
  data. Possible values are "tstatistic", "SAM" (SAM statistical
  score for differential expression), "FC" (Fold Change),
  "FCmedian" (fold change computed on medians)
    }
      \item{logged:}{
  Logical value (default TRUE) to specify if the input data are logged (Log2).
  This parameter will influence the computation of statistics.
    }
      \item{referenceGroupLabel:}{
  Specify which class label is used for the reference sample used
  in computing statistics for differential expression.
    }
      \item{classVector:}{
    If pData_classColumn is NULL then a vector specifying the
  sample  classes is required and can be provided with classVector
  parameter
    }
      \item{testedTail:}{
    Specify what tail of the distribution will be tested for
  significantly extreme values in PREDA analysis. Possible values
  are "both", "upper" or "lower". Default value is "both".
    }
   }
  }
}
\details{
An object of class ExpressionSet is used as input and gene
centered statistics for differential expression are computed on
the contained data. The computed statistics are used to build a
StatisticsForPREDA object
}
\value{
An object of class StatisticsForPREDA
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{StatisticsForPREDA}"}
}
\examples{
  \dontrun{

require(PREDAsampledata)

data(gaExpressionSetRCC)

GEstatisticsForPREDA<-statisticsForPREDAfromEset(
gaExpressionSetRCC, statisticType="tstatistic",
referenceGroupLabel="normal", classVector=sampleinfo[,"Class"])

  }
}

