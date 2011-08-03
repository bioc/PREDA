##
## GenomicAnnotationsForPREDA - GenomicAnnotationsForPREDA2PREDAResults
##
## Method to convert a GenomicAnnotationsForPREDA object to a PREDAResults object
##

setMethod("GenomicAnnotationsForPREDA2PREDAResults", "GenomicAnnotationsForPREDA", function(.Object, analysesNames, testedTail, smoothStatistic, pvalue, qvalue) {

PREDAResults_object<-new("PREDAResults",
ids=slot(.Object,"ids"),
chr=slot(.Object,"chr"),
start=slot(.Object,"start"),
end=slot(.Object,"end"),
strand=slot(.Object,"strand"),
chromosomesNumbers=slot(.Object,"chromosomesNumbers"),
chromosomesLabels=slot(.Object,"chromosomesLabels"),
position=slot(.Object,"position"),
optionalAnnotations=slot(.Object,"optionalAnnotations"),
optionalAnnotationsHeaders=slot(.Object,"optionalAnnotationsHeaders"),
analysesNames=analysesNames,
testedTail=testedTail,
smoothStatistic=smoothStatistic,
pvalue=pvalue,
qvalue=qvalue
)

return(PREDAResults_object)

})

