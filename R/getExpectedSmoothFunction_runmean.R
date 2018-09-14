
getExpectedSmoothFunction_runmean<-function(k.local=10) {

## define the function on the fly, depending on window width k parameter

    PREDA_runmeanStatPerm_fun<-function(input_stat, input_chr, permutePerChromosome=FALSE, permuteStatisticSign=FALSE, input_positions=NULL, output_chr=NULL, output_positions=NULL, k.window=11, ...) {

        if (!is.null(input_positions) & !is.null(output_positions)) {
          if (any(output_positions != input_positions)) {
          stop("runmean does not allow estimating smooothing values at user specified positions.")
          }
        }
      require(caTools)

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

              smoothStat[input_chr==cc]<- runmean(x=current_cc_stat, k=k.window, alg="C", endrule="mean")
              }
          } else {

              if (permuteStatisticSign) {
              permuted_signs<-sample(sign(input_stat))
              input_stat<-(abs(input_stat)*permuted_signs)
              } else {
              input_stat<-sample(input_stat)
              }

              for (cc in input_chr_unique) {
              smoothStat[input_chr==cc]<-  runmean(x=input_stat[input_chr==cc], k=k.window, alg="C", endrule="mean")
              }
          }

      return(smoothStat)
    }

formals(PREDA_runmeanStatPerm_fun)$k.window<-k.local

return(PREDA_runmeanStatPerm_fun)

}
