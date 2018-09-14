
PREDA_splineStat<-function(input_stat, input_chr, input_positions, output_chr, output_positions, output_chr_unique=NULL, ...) {

require(stats)

    if (is.null(output_chr_unique)) {
    output_chr_unique<-unique(output_chr)
    }

    smoothStat<-vector(mode = "numeric", length = length(input_stat))

    for (cc in output_chr_unique) {
    spline_fit<-smooth.spline(x=input_positions[input_chr==cc], y=input_stat[input_chr==cc])$fit
    smoothStat[input_chr==cc]<- predict(spline_fit, x=output_positions[output_chr==cc])$y
    }

return(smoothStat)

}


