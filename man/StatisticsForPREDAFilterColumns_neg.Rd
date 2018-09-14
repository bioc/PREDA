\name{StatisticsForPREDAFilterColumns_neg}
\alias{StatisticsForPREDAFilterColumns_neg}
\alias{StatisticsForPREDAFilterColumns_neg,DataForPREDA-method}
\alias{StatisticsForPREDAFilterColumns_neg,StatisticsForPREDA-method}
\title{
filter statistics to remove selected analyses
}
\description{
filter statistics to remove selected analyses
}
\usage{
# StatisticsForPREDAFilterColumns_neg(.Object, analysesToRemove,
# analysesAsNames=FALSE)

StatisticsForPREDAFilterColumns_neg(.Object, ...)
}
\arguments{
  \item{.Object}{
An object of class StatisticsForPREDA
}
  \item{\dots}{
   See below
   \describe{
        \item{analysesToRemove:}{
      Analysis statistics columns to be removed after filtering
      }
        \item{analysesAsNames:}{
      Logical, if TRUE analyses are listed as their character names.
      If FALSE they can be listed as numeric indexes.
      }
    }
  }

}
