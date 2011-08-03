\name{analysesNames}
\alias{analysesNames}
\alias{analysesNames,StatisticsForPREDA-method}
\alias{analysesNames,PREDAResults-method}
\title{
Get the names of the analyses in the from PREDA objects
}
\description{
Get the names of the analyses in the from StatisticsForPREDA
objects, PREDAResults objects and objects from classes extending
these classes.
}
\usage{
analysesNames(.Object)
}
\arguments{
  \item{.Object}{
an object of class StatisticsForPREDA, PREDAResults or any other
class extending these classes
}
}
\value{
Character vector of analysesNames
}
\author{
Francesco Ferrari
}
\seealso{
\code{"\linkS4class{StatisticsForPREDA}"},
\code{"\linkS4class{PREDAResults}"}
}
\examples{
  require(PREDAsampledata)
  data(SODEGIRGEanalysisResults)
  analysesNames(SODEGIRGEanalysisResults)
}
