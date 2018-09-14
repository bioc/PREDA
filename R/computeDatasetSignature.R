

setMethod("computeDatasetSignature", "GenomicAnnotationsForPREDA", function(.Object, genomicRegionsList=genomicRegionsList, multTestCorrection="fdr", signature_qval_threshold=0.05, returnRegions=TRUE, use.referencePositions=TRUE) {

    OBSERVED_FLAGS<-GenomicAnnotationsForPREDAGetExpectedFlags(.Object, genomicRegionsList=genomicRegionsList)


    # create a fake lap_results object just to manage genomicRegions
    dataset_signature_qvalues<-datasetSignatureFromFlags(OBSERVED_FLAGS, multTestCorrection=multTestCorrection)
    dataset_signature_pvalues<-datasetSignatureFromFlags(OBSERVED_FLAGS, multTestCorrection="none")
    dataset_signature_statistic<-datasetSignatureFromFlags_getFrequencies(OBSERVED_FLAGS, relative=TRUE)


      # this check is not actually required
      if (!(all(rownames(dataset_signature_qvalues) == slot(.Object, "ids")) &
      all(rownames(dataset_signature_pvalues) == slot(.Object, "ids")) &
      all(rownames(dataset_signature_statistic) == slot(.Object, "ids")))) {
      stop("wrong ids alignement")
      }


    PREDAResults_temp_object<-new("PREDAResults",
    ids=slot(.Object, "ids"),
    chr=slot(.Object, "chr"),
    start=slot(.Object, "start"),
    end=slot(.Object, "end"),
    strand=slot(.Object, "strand"),
    chromosomesNumbers=slot(.Object, "chromosomesNumbers"),
    chromosomesLabels=slot(.Object, "chromosomesLabels"),
    position=slot(.Object, "position"),
    optionalAnnotations=slot(.Object, "optionalAnnotations"),
    optionalAnnotationsHeaders=slot(.Object, "optionalAnnotationsHeaders"),
    analysesNames="datasetSignature",
    testedTail="both",
    smoothStatistic=dataset_signature_statistic,
    pvalue=dataset_signature_pvalues,
    qvalue=dataset_signature_qvalues )

    if (returnRegions) {
    Genomic_regions_fromSignature<-PREDAResults2GenomicRegions(PREDAResults_temp_object, qval.threshold=signature_qval_threshold, use.referencePositions=use.referencePositions)
    return(Genomic_regions_fromSignature)
    } else {
    return(PREDAResults_temp_object)
    }

})





