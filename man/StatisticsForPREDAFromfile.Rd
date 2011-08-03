\name{StatisticsForPREDAFromfile}
\alias{StatisticsForPREDAFromfile}
\title{
Function to create a StatisticsForPREDA objet from a txt file
}
\description{
Function to create a StatisticsForPREDA objet from a txt file
}
\usage{
StatisticsForPREDAFromfile(file, ids_column = NULL,
statistic_columns = NULL, analysesNames = NULL, testedTail =
c("upper", "lower", "both"), ...)
}
\arguments{
  \item{file}{
Path to the input txt file containing statistics on genomics data
}
  \item{ids_column}{
Specify the column from the input txt file with gene (or
other genomic features) ids. Can be specified using column
index (numeric) or column name (character).
}
  \item{statistic_columns}{
Specify the column (or columns) from the input txt file with
gsta.enomic data statistics that will be included in the
statisticsForPREDA object. Can be specified using column
index (numeric) or column name (character).

If NULL (default), all columns excluding ids_column will be
considered as input statistics
}
  \item{analysesNames}{
Names (labels) to be associated to each input statistic. If NULL
the column names for statistics_columns will be used.
}
  \item{testedTail}{
  Specify what tail of the distribution will be tested for
significantly extreme values in PREDA analysis. Possible values
are "both", "upper" or "lower".
}
  \item{\dots}{
any other parameter for read.table function that could be useful
for parsing the input file, such as "sep", "quote", "header",
"na.strings" and other parameters.

}
}
\details{
A txt file is parsed and a statisticsForPREDA object is built
using contained data.
}
\value{
A statisticsForPREDA object
}
\author{
Francesco Ferrari
}
\seealso{
\code{\linkS4class{StatisticsForPREDA}}
}
\examples{
  \dontrun{
require(PREDAsampledata)

CNdataPath <- system.file("sampledata", "CopyNumber", package =
"PREDAsampledata")

CNdataFile <- file.path(CNdataPath , "CNAG_data_PREDA.txt")

CNannotationFile <- file.path(CNdataPath , "SNPAnnot100k.csv")

CNStatisticsForPREDA<-StatisticsForPREDAFromfile(file=CNdataFile,
ids_column="AffymetrixSNPsID", testedTail="both", sep="\t",
header=TRUE)

  }
}
