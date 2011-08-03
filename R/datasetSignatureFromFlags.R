### %%% NO EXPORT %%%
### %%% SODEGIR WORKFLOW %%%


datasetSignatureFromFlags<-function(observedFlagsMatrix, selectedFlags=NULL, referenceNullFlag=0, multTestCorrection="none",  selectedSubset=NULL) {

    # check input matrix
    if (!is.matrix(observedFlagsMatrix)) {
        if (is.data.frame(observedFlagsMatrix)) {
        observedFlagsMatrix<-as.matrix(observedFlagsMatrix)
        } else {
        stop("observedFlagsMatrix must be a matrix object")
        }
    }

    # check selected flags for computing frequencies
    if (is.null(selectedFlags)) {
        if (is.null(referenceNullFlag)) {
        stop("Specify either the selectedFlags or the referenceNullFlag argument")
        } else {
        availableFlags<-unique(as.vector(observedFlagsMatrix))
        selectedFlags<-availableFlags[!(availableFlags %in% referenceNullFlag)]
            if (length(selectedFlags)==0) {
            stop("Only one flag has been detected! Relative frequencies of distinct flags can\'t be estimated.")
            }
        }
    }

    # check selected subset
    if (is.null(selectedSubset)) {
    selectedSubset<-(1:ncol(observedFlagsMatrix))
    }

    pvaluesOutputMatrix<-NULL
    # for each selected flag
    for (flagtype in selectedFlags) {
    # computing expected frequency
    expected_frequency<-sum(observedFlagsMatrix==flagtype)/length(observedFlagsMatrix)
    # computing binomial pvalue for over-representation
    p_binomial<-apply(observedFlagsMatrix[,selectedSubset],1, FUN=function(x) {
    return(sum(dbinom(x=(sum(x==flagtype):length(x)), size=length(x), prob=expected_frequency)))
    })

        # multiple testing correction
        if (!(is.null(multTestCorrection))) {
        p_binomial<-PREDA_multTestCorrection(pvalues=p_binomial, method=multTestCorrection)
        }

    # storing output in the matrix of pvalues
    pvaluesOutputMatrix<-cbind(pvaluesOutputMatrix, p_binomial)
    }

    rownames(pvaluesOutputMatrix)<-rownames(observedFlagsMatrix)
    colnames(pvaluesOutputMatrix)<-paste("pvalue", selectedFlags, sep="")

return(pvaluesOutputMatrix)
}









datasetSignatureFromFlags_getFrequencies<-function(observedFlagsMatrix, selectedFlags=NULL, referenceNullFlag=0, relative=FALSE, selectedSubset=NULL) {

    # check input matrix
    if (!is.matrix(observedFlagsMatrix)) {
        if (is.data.frame(observedFlagsMatrix)) {
        observedFlagsMatrix<-as.matrix(observedFlagsMatrix)
        } else {
        stop("observedFlagsMatrix must be a matrix object")
        }
    }

    # check selected flags for computing frequencies
    if (is.null(selectedFlags)) {
        if (is.null(referenceNullFlag)) {
        stop("Specify either the selectedFlags or the referenceNullFlag argument")
        } else {
        availableFlags<-unique(as.vector(observedFlagsMatrix))
        selectedFlags<-availableFlags[!(availableFlags %in% referenceNullFlag)]
            if (length(selectedFlags)==0) {
            stop("Only one flag has been detected! Relative frequencies of distinct flags can\'t be estimated.")
            }
        }
    }

    # check selected subset
    if (is.null(selectedSubset)) {
    selectedSubset<-(1:ncol(observedFlagsMatrix))
    }

    pvaluesOutputMatrix<-NULL
    # for each selected flag
    for (flagtype in selectedFlags) {

        # computing observed frequencies
        p_binomial<-apply(observedFlagsMatrix[,selectedSubset],1, FUN=function(x) {
        return(sum(x==flagtype))
        })
    
        # reporting frequency as relative frequency
        if (relative) {
        p_binomial<-(p_binomial/ncol(observedFlagsMatrix))
        }
    
        # storing output in the matrix of pvalues
        pvaluesOutputMatrix<-cbind(pvaluesOutputMatrix, p_binomial)
    }

    rownames(pvaluesOutputMatrix)<-rownames(observedFlagsMatrix)
    colnames(pvaluesOutputMatrix)<-paste("frequency", selectedFlags, sep="")

return(pvaluesOutputMatrix)
}




