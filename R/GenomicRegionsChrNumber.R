
##
## method GenomicRegionsChrNumber to extract number of chromosomes
##

##
## GenomicRegions - GenomicRegionsChrNumber
##

setMethod("GenomicRegionsChrNumber", "GenomicRegions", function(.Object) {
Chrs<-slot(.Object, "chr")
ChrNumber<-length(unique(Chrs))
return(ChrNumber)
})

