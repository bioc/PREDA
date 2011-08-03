### %%% HIDDEN %%% ###

####
#### GE_simulations_samplingColumns : function for shuffling columns from an expression set
####


setMethod("GE_simulations_samplingColumns", "ExpressionSet", function(.Object, pData_classColumn=NULL) {

require(Biobase)

  if ((is.null(pData_classColumn)) | (!(pData_classColumn %in% colnames(pData(.Object))))) {
  stop("Please specify a valid phenodata column containing sample classes")
  }

  # extract information about sample classes
  classFactor<-(pData(.Object)[,pData_classColumn])

  # observed data matrix
  datamatrix<-exprs(.Object)

  # shuffling data
  datamatrix_shuffled<-GE_simulations_samplingColumns_onMatrix(datamatrix=datamatrix, classFactor=classFactor)

  # modifying expressionSet
  exprs(.Object)<-datamatrix_shuffled

return(.Object)
})



####
#### GE_simulations_samplingColumns_onMatrix : same function as above but active on generic matrices
####

GE_simulations_samplingColumns_onMatrix<-function(datamatrix, classFactor) {

classFactor<-as.factor(classFactor)

  # check if we have two and only two sample classes
  if (length(levels(classFactor)) != 2) {
  stop("This function was developed to shuffle samples pertaining just to 2 classes!!")
  }

  # sampling data columns 
  permVector<-getPermutationMatrix(n1=table(classFactor)[1], n2=table(classFactor)[2], skip_observed=TRUE, perm_number=1)

  # shuffled data matrix
  original_names<-colnames(datamatrix)
  datamatrix<-datamatrix[,permVector]
  colnames(datamatrix)<-original_names

return(datamatrix)
}





####
#### getPermutationMatrix : function for obtaining permutation matrices
####


getPermutationMatrix<-function(n1, n2, skip_observed=TRUE, perm_number=NULL) {
n<-n1+n2
ss<-1:n
B1<-combn(ss, min(n1,n2))
B2<-apply(B1,2,function(x){ss[-x]})
B<-rbind(B1,B2)
B<-t(B)

  if (skip_observed) {
  B<-B[-1,]
  }

  if (!(is.null(perm_number))) {
    if (perm_number <= nrow(B)) {
    sampling_vector<-sample(nrow(B), size=perm_number, replace=FALSE)
    B<-B[sampling_vector,]
    } else {
    warning("Resampling columns with replacement due to high permutation number required!!")
    sampling_vector<-sample(nrow(B), size=perm_number, replace=TRUE)
    B<-B[sampling_vector,]
    }
  }

return(B)

}




####
#### GE_sample_rows: function to sample randomly data from rows: so as to select just one element from each row
####


GE_sample_rows<-function(datamatrix) {
  sampled_vector<-apply(datamatrix, 1, FUN=function(x) {
  return(sample(x, size=1))
  })
return(sampled_vector)
}



