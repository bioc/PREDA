PREDA_multTestCorrection<-function(pvalues, method) {

    if (method=="qvalue") {

        require(qvalue)
        corrected_pvalues<-(qvalue(pvalues)$qvalues)
        return(corrected_pvalues)

    } else if (method=="fdr") {

        require(multtest)
        adjusted.pvalues<-mt.rawp2adjp(pvalues, proc=c("BH"))
        # riporto i pValue aggiustati nel loro ordine originale
        ordering.vector<-order(adjusted.pvalues$index)
        # vector of q-values
        FDR<-adjusted.pvalues$adjp[ordering.vector,"BH"]
        return(FDR)

    } else if (method=="none") {
    return(pvalues)
    } else {

        stop("Only \"qvalue\", \"fdr\" or \"none\" are allowed as multiple testing correction methods.")

    }

}


