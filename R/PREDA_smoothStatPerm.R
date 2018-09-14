

PREDA_smoothStatPerm<-function(input_stat, input_chr, input_positions, output_chr, output_positions, output_chr_unique=NULL, permutePerChromosome=FALSE, permuteStatisticSign=FALSE, bandwidth=NULL, lokern_scaledBandwidth_repeated=FALSE, lokern_scaledBandwidthFactor=1, ...) {

require(lokern)

    if (is.null(output_chr_unique)) {
    output_chr_unique<-unique(output_chr)
    }

    smoothStat<-vector(mode = "numeric", length = length(output_positions))

    # the permutePerChromosome paramteres allows to decide if permutations should be performed on each chromosome or on global vector of statistics
    if(permutePerChromosome) {
        for (cc in output_chr_unique) {

          if (permuteStatisticSign) {
          current_signs<-sample(sign(input_stat[input_chr==cc]))
          current_cc_stat<-(abs(input_stat[input_chr==cc])*current_signs)
          } else {
          current_cc_stat<-sample(input_stat[input_chr==cc])
          }

          if (lokern_scaledBandwidth_repeated) {
            current_bandwidth<- lokerns(x=input_positions[input_chr==cc], y=current_cc_stat, x.out=output_positions[output_chr==cc])$bandwidth
            current_bandwidth<-trunc(current_bandwidth/lokern_scaledBandwidthFactor)
            smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=current_cc_stat, x.out=output_positions[output_chr==cc], bandwidth=current_bandwidth)$est
          } else {
          smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=current_cc_stat, x.out=output_positions[output_chr==cc], bandwidth=bandwidth[output_chr==cc])$est
          }

        }
    } else {

        if (permuteStatisticSign) {
        permuted_signs<-sample(sign(input_stat))
        input_stat<-(abs(input_stat)*permuted_signs)
        } else {
        input_stat<-sample(input_stat)
        }

        if (lokern_scaledBandwidth_repeated) {
          for (cc in output_chr_unique) {
          current_bandwidth<- lokerns(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc], x.out=output_positions[output_chr==cc])$bandwidth
          current_bandwidth<-trunc(current_bandwidth/lokern_scaledBandwidthFactor)
          smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc], x.out=output_positions[output_chr==cc], bandwidth=current_bandwidth)$est
          }
        } else {
          for (cc in output_chr_unique) {
          smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc], x.out=output_positions[output_chr==cc], bandwidth=bandwidth[output_chr==cc])$est
          }
        }
    }

return(smoothStat)

}


