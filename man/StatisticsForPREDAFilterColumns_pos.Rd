\name{StatisticsForPREDAFilterColumns_pos}
\alias{StatisticsForPREDAFilterColumns_pos}
\alias{StatisticsForPREDAFilterColumns_pos,DataForPREDA-method}
\alias{StatisticsForPREDAFilterColumns_pos,StatisticsForPREDA-method}
\title{
filter statistics to keep selected analyses
}
\description{
filter statistics to keep selected analyses
}
\usage{
# StatisticsForPREDAFilterColumns_pos(.Object, analysesToRetain,
# analysesAsNames=FALSE)

StatisticsForPREDAFilterColumns_pos(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class StatisticsForPREDA
}
  \item{\dots}{
   See below
   \describe{
  \item{analysesToRetain:}{
Analysis statistics columns to be retained after filtering
}
  \item{analysesAsNames:}{
Logical, if TRUE analyses are listed as their character names.
If FALSE they can be listed as numeric indexes.
}
      }
  }
}
