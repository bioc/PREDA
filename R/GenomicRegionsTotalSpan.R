##
## method GenomicRegionsTotalSpan 
##

##
## GenomicRegions - GenomicRegionsTotalSpan
##

setMethod("GenomicRegionsTotalSpan", "GenomicRegions", function(.Object, unit=c("bp","Kb","Mb"), digits=2, outputNumeric=FALSE) {

spans_single_regions<-GenomicRegionsSpan(.Object, unit="bp", outputNumeric=TRUE)
spans<-sum(spans_single_regions)

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


