## 
## method PREDAResults2PREDADataAndResults to filter annotations to KEEP selected chromosomes
## 

## 
## PREDAResults - PREDAResults2PREDADataAndResults
## 


setMethod("PREDAResults2PREDADataAndResults", "PREDAResults", function(.Object, statistic) {

if (any(rownames(statistic)!=slot(.Object,"ids"))) {
stop("statistic matrix rownames doesn\' correspond to ids of PREDAResults object")
}


PREDADataAndResults<-new("PREDADataAndResults",
ids=slot(.Object,"ids"),
chr=slot(.Object,"chr"),
start=slot(.Object,"start"),
end=slot(.Object,"end"),
strand=slot(.Object,"strand"),
chromosomesNumbers=slot(.Object,"chromosomesNumbers"),
chromosomesLabels=slot(.Object,"chromosomesLabels"),
position=slot(.Object,"position"),
optionalAnnotations=slot(.Object, "optionalAnnotations"),
optionalAnnotationsHeaders=slot(.Object, "optionalAnnotationsHeaders"),
analysesNames=slot(.Object,"analysesNames"),
testedTail=slot(.Object,"testedTail"),
smoothStatistic=slot(.Object,"smoothStatistic"),
pvalue=slot(.Object,"pvalue"),
qvalue=slot(.Object,"qvalue"),
statistic=statistic
)

return(PREDADataAndResults)

})

