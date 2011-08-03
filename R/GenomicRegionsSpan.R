##
## method GenomicRegionsSpan to extract regions span
##

##
## GenomicRegions - GenomicRegionsSpan
##

setMethod("GenomicRegionsSpan", "GenomicRegions", function(.Object, unit=c("bp","Kb","Mb"), digits=2, outputNumeric=FALSE) {
starts<-slot(.Object, "start")
ends<-slot(.Object, "end")
spans<-(ends-starts)

    if (unit=="bp") {
        if (!outputNumeric) {
        spans<-paste(spans, "bp")
        }
    return(spans)
    } else if ((unit=="Kb") | (unit=="kb") | (unit=="KB") | (unit=="kB") ) {
    spans<-round((spans/1000), digits=digits)
        if (!outputNumeric) {
        spans<-paste(spans, "Kb")
        }
    return(spans)
    } else if ((unit=="Mb") | (unit=="mb") | (unit=="MB") | (unit=="mB") ) {
    spans<-round((spans/1000000), digits=digits)
        if (!outputNumeric) {
        spans<-paste(spans, "Mb")
        }
    return(spans)
    } else {
    stop("Invalid unit value type")
    }

})

