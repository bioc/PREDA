##
## method GenomicRegionsCreateRegionsIds to create values for ids slot
##

##
## GenomicRegions - GenomicRegionsCreateRegionsIds
##

setMethod("GenomicRegionsCreateRegionsIds", "GenomicRegions", function(.Object, chromosomePrefix="chr", chr2span.sep=":", span.sep="-") {
chr<-slot(.Object, "chr")
start<-slot(.Object, "start")
end<-slot(.Object, "end")

ids<-paste(rep(chromosomePrefix, times=length(chr)), chr, rep(chr2span.sep, times=length(chr)), start, rep(span.sep, times=length(chr)), end)
.Object@ids <- as.character(ids)

return(.Object)

})



