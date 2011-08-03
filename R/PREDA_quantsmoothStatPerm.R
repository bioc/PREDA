

PREDA_quantsmoothStatPerm<-function(input_stat, input_chr, permutePerChromosome=FALSE, permuteStatisticSign=FALSE, input_positions=NULL, output_chr=NULL, output_positions=NULL, ...) {


  if (!is.null(input_positions) & !is.null(output_positions)) {
    if (any(output_positions != input_positions)) {
    stop("current implementation of quantsmooth does not allow estimating smooothing values at user specified positions.")
    }
  }

    require(quantsmooth)

    input_chr_unique<-unique(input_chr)

    smoothStat<-vector(mode = "numeric", length = length(input_stat))


    if(permutePerChromosome) {
        for (cc in input_chr_unique) {

          if (permuteStatisticSign) {
          current_signs<-sample(sign(input_stat[input_chr==cc]))
          current_cc_stat<-(abs(input_stat[input_chr==cc])*current_signs)
          } else {
          current_cc_stat<-sample(input_stat[input_chr==cc])
          }

        smoothStat[input_chr==cc]<- quantsmooth(intensities=current_cc_stat)
        }
    } else {

        if (permuteStatisticSign) {
        permuted_signs<-sample(sign(input_stat))
        input_stat<-(abs(input_stat)*permuted_signs)
        } else {
        input_stat<-sample(input_stat)
        }

        for (cc in input_chr_unique) {
        smoothStat[input_chr==cc]<- quantsmooth(intensities=input_stat[input_chr==cc])
        }

    }

return(smoothStat)

}


