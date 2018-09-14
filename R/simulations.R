##
## DataForPREDARandomShuffle: this function is used to randomly shuffle data from a data for PREDA object in order to remove any local dependency between data
##


setMethod("DataForPREDARandomShuffle", "DataForPREDA", function(.Object, randomize.perchromosome=FALSE, randomize.columns=TRUE, randomize.rows=FALSE) {

  if (!any(c(randomize.perchromosome, randomize.columns, randomize.rows))) {
  print("Nothing to randomize")
  return(.Object)
  }

  # if randomize.chromosome is true and randomize.colums is FALSE, we assume that the parameter randomize column was improperly set
  if (randomize.perchromosome & (!(randomize.columns))) {
  randomize.columns<-TRUE
  }


statistic.matrix<-slot(.Object, "statistic")

  if (randomize.columns) {
    if (!(randomize.perchromosome)) {
    statistic.matrix.shuffled<-apply(statistic.matrix,2,sample)
    } else {
    chr<-slot(.Object, "chr")
    statistic.matrix.shuffled<- statistic.matrix
      for (cc in unique(chr)) {        
      statistic.matrix.shuffled[which(chr==cc),]<-apply(statistic.matrix[which(chr==cc),],2,sample)
      }
    }
  statistic.matrix<-statistic.matrix.shuffled
  }


  if (randomize.rows) {
  statistic.matrix<-t(apply(statistic.matrix,1,sample))
  }

rownames(statistic.matrix)<-slot(.Object, "ids")
colnames(statistic.matrix)<-slot(.Object, "analysesNames")

slot(.Object, "statistic")<-statistic.matrix

return(.Object)

})



##
## DataForPREDAAddEffect_single: this function is used to add a simulated effect to asingle chromsomal region on a DataForPREDA object
##



setMethod("DataForPREDAAddEffect_single", "DataForPREDA", function(.Object, chr, start, end=NULL, affected.features=NULL, effect=NULL, additive.effect=TRUE, selected.samples=NULL, effect.noise=FALSE) {

  if (is.null(effect)) {
  stop("Please define the effect parameter")
  }


if ((is.null(end) & is.null(affected.features)) | ((!is.null(end)) & (!is.null(affected.features)))) {
stop("Please specify either the region end or the number of adjacent affected features but not both of them")
} else if (!is.null(end) & is.null(affected.features)) {
feature_selection<-which((slot(.Object, "chr")==chr) & (slot(.Object, "position")>=start) & (slot(.Object, "position")<=end))
} else if (is.null(end) & (!is.null(affected.features))) {
starting.point<-min(slot(.Object, "position")[((slot(.Object, "chr")==chr) & (slot(.Object, "position")>=start))])
currentChr_unique_positions<-unique(sort(slot(.Object, "position")[(slot(.Object, "chr")==chr)]))
starting.point.id<-which(currentChr_unique_positions==starting.point)
feature_position_selection<-currentChr_unique_positions[starting.point.id:(starting.point.id + affected.features -1)]
feature_selection<-which((slot(.Object, "chr")==chr) & (slot(.Object, "position") %in% feature_position_selection))
}



  if (is.null(selected.samples)) {
  selected.samples<-1:length(slot(.Object, "analysesNames"))
  }


statistic.matrix<-slot(.Object, "statistic")

    if (effect.noise) {
      effect.matrix<-matrix(0, nrow=length(feature_selection), ncol=length(selected.samples))
      for (selected.sample.index in 1:length(selected.samples)) {
#      effect.matrix[,selected.sample.index]<-rnorm(n=length(feature_selection), mean = effect, sd = sd(statistic.matrix[,selected.samples[selected.sample.index]]))
      effect.matrix[,selected.sample.index]<-runif(n=length(feature_selection), min=min(0, effect), max = max(0, effect))
      }
    effect<-effect.matrix
    }

    if (additive.effect) {
    statistic.matrix[feature_selection,selected.samples]<-(statistic.matrix[feature_selection,selected.samples] + effect)
    } else {
    statistic.matrix[feature_selection,selected.samples]<-(statistic.matrix[feature_selection,selected.samples] * effect)
    }



slot(.Object, "statistic")<-statistic.matrix

return(.Object)

})







##
## DataForPREDAAddEffects: this function is used to add a simulated effect to specified GenomicRegions on a DataForPREDA object
##



setMethod("DataForPREDAAddEffects", c("DataForPREDA", "GenomicRegions"), function(.Object1, .Object2, affected.features=NULL, effect=NULL, additive.effect=TRUE, selected.samples=NULL, effect.noise=FALSE) {

   for (region.index in 1:GenomicRegionsNumber(.Object2)) {
      if (is.null(affected.features)) {
      end<-slot(.Object2, "end")[region.index]
      } else {
      end<-NULL
      }
   .Object1<-DataForPREDAAddEffect_single(.Object1, chr=slot(.Object2, "chr")[region.index], start=slot(.Object2, "start")[region.index], end=end, affected.features=affected.features, effect=effect, additive.effect=additive.effect, selected.samples=selected.samples, effect.noise=effect.noise)
   }

return(.Object1)

})


##
## DataForPREDASimulate: this function is used to add a simulated effect to specified GenomicRegions on a DataForPREDA object and to perform additional optional shuffling of data
##


setMethod("DataForPREDASimulate", c("DataForPREDA", "GenomicRegions"), function(.Object1, .Object2, effect=NULL, additive.effect=TRUE, affected.features=NULL, selected.samples=NULL, effect.noise=FALSE, shuffle.data=TRUE, randomize.perchromosome=FALSE, randomize.columns=TRUE, randomize.rows=FALSE) {

  if (shuffle.data) {
  .Object1<-DataForPREDARandomShuffle(.Object1, randomize.perchromosome=randomize.perchromosome, randomize.columns=randomize.columns, randomize.rows=randomize.rows)
  }

  .Object1<-DataForPREDAAddEffects(.Object1, .Object2, affected.features=affected.features, effect=effect, additive.effect=additive.effect, selected.samples=selected.samples, effect.noise=effect.noise)

return(.Object1)

})




##
## DataForPREDASimulationGetExpectedFlags: this function is used to retrieve a matrix indicating which feature are expeted to be positive after PREDA analysis in a simulation study
##



setMethod("DataForPREDASimulationGetExpectedFlags", c("DataForPREDA", "GenomicRegions"), function(.Object1, .Object2, null.value=0, significant.value=1, affected.features=NULL, selected.samples=NULL) {

 if (is.null(selected.samples)) {
  selected.samples<-1:length(slot(.Object1, "analysesNames"))
  }


ExpectedFlags.matrix<-matrix(null.value, nrow=length(slot(.Object1,"ids")), ncol=length(slot(.Object1,"analysesNames")))


   for (region.index in 1:GenomicRegionsNumber(.Object2)) {
   chr<-slot(.Object2, "chr")[region.index]
   start<-slot(.Object2, "start")[region.index]
   end<-slot(.Object2, "end")[region.index]
      if (is.null(affected.features)) {
      feature_selection<-which((slot(.Object1, "chr")==chr) & (slot(.Object1, "position")>=start) & (slot(.Object1, "position")<=end))
      } else {
      starting.point<-min(slot(.Object1, "position")[((slot(.Object1, "chr")==chr) & (slot(.Object1, "position")>=start))])
      currentChr_unique_positions<-unique(sort(slot(.Object1, "position")[(slot(.Object1, "chr")==chr)]))
      starting.point.id<-which(currentChr_unique_positions==starting.point)
      feature_position_selection<-currentChr_unique_positions[starting.point.id:(starting.point.id + affected.features -1)]
      feature_selection<-which((slot(.Object1, "chr")==chr) & (slot(.Object1, "position") %in% feature_position_selection))
      }
   ExpectedFlags.matrix[feature_selection,selected.samples]<-significant.value
   }

colnames(ExpectedFlags.matrix)<-slot(.Object1, "analysesNames")
rownames(ExpectedFlags.matrix)<-slot(.Object1, "ids")

return(ExpectedFlags.matrix)

})




##
## DataForPREDAMedianCenter: this function is used to center statistics (per column) around the median
##


setMethod("DataForPREDAMedianCenter", "DataForPREDA", function(.Object, use.mean=FALSE, additive=TRUE) {

statistic.matrix<- slot(.Object,"statistic")

  if (use.mean) {
  medians.vector<-apply(statistic.matrix, 2, mean)
  } else {
  medians.vector<-apply(statistic.matrix, 2, median)
  }

  if (additive) {
  statistic.matrix<-t(t(statistic.matrix)-medians.vector)
  } else {
  statistic.matrix<-t(t(statistic.matrix)/medians.vector)
  }

slot(.Object,"statistic")<-statistic.matrix

return(.Object)

})





###
### EvaluateSimulationPerformance
###


EvaluateSimulationPerformance<-function(observedFlags, expectedFlags, null.value=0, significant.value=1) {


TP<-NULL
TN<-NULL
FP<-NULL
FN<-NULL
  for (i in 1:ncol(observedFlags)) {
  TP<-c(TP, sum((expectedFlags[,i] != null.value) & (observedFlags[,i] != null.value)))
  TN<-c(TN, sum((expectedFlags[,i] == null.value) & (observedFlags[,i] == null.value)))
  FP<-c(FP, sum((expectedFlags[,i] == null.value) & (observedFlags[,i] != null.value)))
  FN<-c(FN, sum((expectedFlags[,i] != null.value) & (observedFlags[,i] == null.value)))
  }
SENSITIVITY <- (TP/(TP+FN))
SPECIFICITY <- (TN/(FP+TN))
FDR <- (FP/(FP + TP))
FNR <- (FN/(FN + TN))


# names(TP)<-colnames(observedFlags)
# names(TN)<-colnames(observedFlags)
# names(FP)<-colnames(observedFlags)
# names(FN)<-colnames(observedFlags)
# names(SENSITIVITY)<-colnames(observedFlags)
# names(SPECIFICITY)<-colnames(observedFlags)
# names(FDR)<-colnames(observedFlags)
# names(FNR)<-colnames(observedFlags)


output_stats<-data.frame(TP, TN, FP, FN, SENSITIVITY, SPECIFICITY, FDR, FNR)
rownames(output_stats)<-colnames(observedFlags)

return(output_stats)

}



