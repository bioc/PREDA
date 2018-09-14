

PREDA_quantsmoothStat<-function(input_stat, input_chr, input_positions=NULL, output_chr=NULL, output_positions=NULL, ...) {

  if (!is.null(input_positions) & !is.null(output_positions)) {
    if (any(output_positions != input_positions)) {
    stop("current implementation of quantsmooth does not allow estimating smooothing values at user specified positions.")
    }
  }

require(quantsmooth)

    input_chr_unique<-unique(input_chr)

    smoothStat<-vector(mode = "numeric", length = length(input_stat))
    for (cc in input_chr_unique) {
    smoothStat[input_chr==cc]<- quantsmooth(intensities=input_stat[input_chr==cc])
    }

return(smoothStat)

}


