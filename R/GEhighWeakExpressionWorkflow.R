### %%% TMAPPING WORKFLOW %%% 


###
### This function is used to obtain another function taht can be used to trasform a data matrix into a standardized matrix of data
###


getStardadizeFunction<-function(simulation.statistic_type=NULL) {

    if (simulation.statistic_type=="zscore") {
      my_standardize<-function(x) {
      return((x-mean(x))/sd(x))
      }
    } else if (simulation.statistic_type=="zscoreCV") {
      my_standardize<-function(x) {
      return((x-mean(x))/mean(x))
      }
    } else if (simulation.statistic_type=="FC") {
      my_standardize<-function(x) {
      return(x-mean(x))
      }
    } else if (simulation.statistic_type=="FCmedian") {
      my_standardize<-function(x) {
      return(x-median(x))
      }
    } else if (simulation.statistic_type=="squaredDelta") {
      my_standardize<-function(x) {
      scarto<-(x-median(x))
      return((scarto^2)*sign(scarto))
      }
    } else {
      stop("Please specify a valid simulation.statistic_type")
    }

return(my_standardize)
}





###
### This function can be used to trasform an expressionset into a standardized expressionset
###

setMethod("GE_standardize", "ExpressionSet", function(.Object, simulation.statistic_type="zscore") {

  # data matrix
  datamatrix<-exprs(.Object)

  my_standardize<-getStardadizeFunction(simulation.statistic_type=simulation.statistic_type)

  datamatrix_standardized<-t(apply(datamatrix, 1, FUN=my_standardize))

  # modifying expressionSet
  exprs(.Object)<-datamatrix_standardized

return(.Object)

})




###
### This function can be used to trasform a StatisticsForPREDA object into a standardized StatisticsForPREDA object
###


setMethod("GE_standardize", "StatisticsForPREDA", function(.Object, simulation.statistic_type="zscore") {

  # data matrix
  datamatrix<-slot(.Object, "statistic")

  my_standardize<-getStardadizeFunction(simulation.statistic_type=simulation.statistic_type)

  datamatrix_standardized<-t(apply(datamatrix, 1, FUN=my_standardize))

  # modifying slot
  .Object@statistic<-datamatrix_standardized

return(.Object)

})




