
getObservedSmoothFunction_runmean<-function(k.local=10) {

## define the function on the fly, depending on window width k parameter

    PREDA_runmeanStat_fun<-function(input_stat, input_chr, input_positions=NULL, output_chr=NULL, output_positions=NULL, k.window=11, ...) {
        if (!is.null(input_positions) & !is.null(output_positions)) {
          if (any(output_positions != input_positions)) {
          stop("runmean does not allow estimating smooothing values at user specified positions.")
          }
        }
      require(caTools)
          input_chr_unique<-unique(input_chr)
          smoothStat<-vector(mode = "numeric", length = length(input_stat))
          for (cc in input_chr_unique) {
          smoothStat[input_chr==cc]<- runmean(x=input_stat[input_chr==cc], k=k.window, alg="C", endrule="mean")
          }
      return(smoothStat)
    }

    formals(PREDA_runmeanStat_fun)$k.window<-k.local

return(PREDA_runmeanStat_fun)

}
