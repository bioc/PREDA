\name{eset2GenomicAnnotations}
\alias{eset2GenomicAnnotations}
\alias{eset2GenomicAnnotations,ExpressionSet-method}
\title{
Function building a GenomicAnnotations object on an ExpressionSet object
}
\description{
Function building a GenomicAnnotations object on an ExpressionSet object
}
\usage{
# eset2GenomicAnnotations(.Object, retain.chrs,
# optionalAnnotations)

eset2GenomicAnnotations(.Object, ...)
}
\arguments{
  \item{.Object}{
ExpressionSet object. The associated annotation library will be
used to build a GenomicAnnotations object.
}
  \item{\dots}{
  See below
   \describe{
        \item{retain.chrs:}{
      Numeric vector, containing the list of chromosomes selected for
      the output GenomicAnnotations object. E.g. set retain.chrs=1:22
      to limit the GenomicAnnotations object to chromosomes from 1 to
      22. This might be ueseful to limit GenomiAnnotations objects to
      autosomic chromosomes.
      }
        \item{optionalAnnotations:}{
      Character vector to select additional annotations fields to be
      included into the GenomicAnnotations object.
      }
    }
  }
}
\value{
An object of class \code{"\linkS4class{GenomicAnnotations}"}
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{GenomicAnnotations}"}
}
\examples{
  \dontrun{
require("PREDAsampledata")
data(gaExpressionSetRCC)

GEGenomicAnnotations<-eset2GenomicAnnotations(gaExpressionSetRCC,
retain.chrs=1:22)

  }
}

