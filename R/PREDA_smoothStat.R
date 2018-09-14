

PREDA_smoothStat<-function(input_stat, input_chr, input_positions, output_chr, output_positions, output_chr_unique=NULL, returnBandwidth=FALSE, bandwidth=NULL, ...) {

require(lokern)

    if (is.null(output_chr_unique)) {
    output_chr_unique<-unique(output_chr)
    }

    smoothStat<-vector(mode = "numeric", length = length(output_positions))

    if (returnBandwidth) {
        for (cc in output_chr_unique) {
        smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc], x.out=output_positions[output_chr==cc])$bandwidth
        }
    } else {
        for (cc in output_chr_unique) {
        smoothStat[output_chr==cc]<- lokerns(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc], x.out=output_positions[output_chr==cc], bandwidth=bandwidth[(output_chr==cc)])$est
        }
    }

return(smoothStat)
}


