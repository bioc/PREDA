\name{PREDA_main}
\alias{PREDA_main}
\title{
function performing the core of PREDA analysis
}
\description{
function performing the core of PREDA analysis
}
\usage{
PREDA_main(inputDataForPREDA, outputGenomicAnnotationsForPREDA
=NULL, nperms = 10000, verbose = TRUE, parallelComputations =
FALSE, multTestCorrection = "fdr", permutePerChromosome = FALSE,
blocksize = 10, permuteStatisticSign = FALSE, smoothMethod =
"lokern_scaledBandwidth_repeated", force = FALSE,
lokern_scaledBandwidthFactor = 2, limit.analysis = NULL)
}
\arguments{
  \item{inputDataForPREDA}{
A Data for PREDA object
}
  \item{outputGenomicAnnotationsForPREDA}{
A GenomicAnnotationsForPREDA object.

If NULL, GenomicsAnnotations for output data are obtained from
inputDataForPREDA
}
  \item{nperms}{
Number of permutations performed in PREDA analysis.
}
  \item{verbose}{
Logical, if TRUE some messages are printed concenrning the
advancement of the analysis.
}
  \item{parallelComputations}{
Logical, if TRUE Rmpi is used to spawn slave processes, thus
using parallel computing to speedup the analysis.
}
  \item{multTestCorrection}{
Multiple testing correction that will be adopted to correct the
statistic p-values. Possible values are "fdr", for benjamini and
Hochberg multiple testing correction and "qvalue" for p-values
correction performed with qvalue package.
}
  \item{permutePerChromosome}{
Logical, if TRUE data parmutations are perfored separatedly for
each chromsoome.
In most cases the default value (FALSE) is preferable to avoid
biases related to specific chromosomes extreme alterations.
}
  \item{blocksize}{
A parameter used to tune parallel computations if
parallelComputations is TRUE. This is actually the number of
permutations performed on each slave process before every
communication with master process.

This is useftul to reduce the numebr of network communications
when slow communicatinos are established among slave processes.
}
  \item{permuteStatisticSign}{
Logical, if TRUE statistics signs are permuted instead of
permuting data along chromsomal position.
}
  \item{smoothMethod}{
The deafault smoothing metod used in the PREDA_main function is
lokern smoothing with scaled bandwidth, using a scaling factor
equal to 2.

Possible values are "lokern", for standard
lokern smoothing, "quantsmooth", "spline" and
"runningmean.x", where x is a user defined value for the number
of adjacent data points using for running mean smoothing.
}
  \item{force}{
Logical, if TRUE force skipping quantsmooth control on
number of data points. Singe quantsmooth is very slow with a
high number of inpuit data, a check stopping computation with
more than 2000 data points in one or more chromosome was
introduced. This aprameter allow skippin this security check.
}
  \item{lokern_scaledBandwidthFactor}{
Factor of scaling for lokern estimated bandwidths
}
  \item{limit.analysis}{
Vector (numeric or character representing analyses names) to
limit the output of preda analysis to a subset of input analyses.
}
}
\details{
See supplementary material about PREDA method
}
\value{

If outputGenomicAnnotationsForPREDA is NULL, a
PREDADataAndResults object is returned. Otherwise a PREDAResults
object is returned instead

}
\author{
Francesco Ferrari
}
\seealso{
Supplementary information about PREDA method
}
\examples{
#See examples in PREDA tutorial
}


