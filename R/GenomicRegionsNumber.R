##
## method GenomicRegionsNumber to extract regions number
##

##
## GenomicRegions - GenomicRegionsNumber
##

setMethod("GenomicRegionsNumber", "GenomicRegions", function(.Object) {
Chrs<-slot(.Object, "chr")
RegionsNumber<-length(Chrs)
return(RegionsNumber)
})
