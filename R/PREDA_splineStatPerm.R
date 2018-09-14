
PREDA_splineStatPerm<-function(input_stat, input_chr, input_positions, output_chr, output_positions, output_chr_unique=NULL, permutePerChromosome=FALSE, permuteStatisticSign=FALSE, ...) {

require(stats)

    if (is.null(output_chr_unique)) {
    output_chr_unique<-unique(output_chr)
    }

    smoothStat<-vector(mode = "numeric", length = length(output_positions))

    if(permutePerChromosome) {
        for (cc in output_chr_unique) {

          if (permuteStatisticSign) {
          current_signs<-sample(sign(input_stat[input_chr==cc]))
          current_cc_stat<-(abs(input_stat[input_chr==cc])*current_signs)
          } else {
          current_cc_stat<-sample(input_stat[input_chr==cc])
          }

          spline_fit<-smooth.spline(x=input_positions[input_chr==cc], y=current_cc_stat)$fit
          smoothStat[output_chr==cc]<- predict(spline_fit, x=output_positions[output_chr==cc])$y

        }
    } else {

        if (permuteStatisticSign) {
        permuted_signs<-sample(sign(input_stat))
        input_stat<-(abs(input_stat)*permuted_signs)
        } else {
        input_stat<-sample(input_stat)
        }

          for (cc in output_chr_unique) {
            spline_fit<-smooth.spline(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc])$fit
            smoothStat[input_chr==cc]<- predict(spline_fit, x=output_positions[output_chr==cc])$y
          }

    }

return(smoothStat)

}

