###############################################################################
###############################################################################
##
## SETTING ALL GENERICS
##
###############################################################################
###############################################################################


##
## Generic GenomicAnnotations2dataframe: extract annotations as a dataframe with probeids as rownames
##

if (!isGeneric("GenomicAnnotations2dataframe")) {
   if (is.function("GenomicAnnotations2dataframe")) {
      fun<-GenomicAnnotations2dataframe
   } else {
      fun<-function(.Object) standardGeneric("GenomicAnnotations2dataframe")
   }
   setGeneric("GenomicAnnotations2dataframe", fun)
}


##
## Generic GenomicAnnotationsSortAndCleanNA: sort annotations according to selected chromosomes and to remove genes containing any NA annotation field
##

if (!isGeneric("GenomicAnnotationsSortAndCleanNA")) {
   if (is.function("GenomicAnnotationsSortAndCleanNA")) {
       fun<-GenomicAnnotationsSortAndCleanNA
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsSortAndCleanNA")
   }
   setGeneric("GenomicAnnotationsSortAndCleanNA", fun)
}


##
## Generic GenomicAnnotations2reference_positions: extract from the genomic Annotations object a vector containing a vector with reference positions
##

if (!isGeneric("GenomicAnnotations2reference_positions")) {
   if (is.function("GenomicAnnotations2reference_positions")) {
       fun<-GenomicAnnotations2reference_positions
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotations2reference_positions")
   }
   setGeneric("GenomicAnnotations2reference_positions", fun)
}


##
## Generic GenomicAnnotationsFilter_neg: filter annotations to remove selected chromosomes
##

if (!isGeneric("GenomicAnnotationsFilter_neg")) {
   if (is.function("GenomicAnnotationsFilter_neg")) {
       fun<-GenomicAnnotationsFilter_neg
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsFilter_neg")
   }
   setGeneric("GenomicAnnotationsFilter_neg", fun)
}


##
## Generic GenomicAnnotationsFilter_pos: filter annotations to KEEP selected chromosomes
##

if (!isGeneric("GenomicAnnotationsFilter_pos")) {
   if (is.function("GenomicAnnotationsFilter_pos")) {
       fun<-GenomicAnnotationsFilter_pos
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsFilter_pos")
   }
   setGeneric("GenomicAnnotationsFilter_pos", fun)
}


##
## Generic GenomicAnnotationsExtract: extract optional annotations for a specific region
##

if (!isGeneric("GenomicAnnotationsExtract")) {
   if (is.function("GenomicAnnotationsExtract")) {
       fun<-GenomicAnnotationsExtract
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsExtract")
   }
   setGeneric("GenomicAnnotationsExtract", fun)
}


##
## Generic GenomicAnnotations2GenomicAnnotationsForPREDA: generate a new GenomicAnnotationsForPREDA object from a GenomicAnnotations object
##

if (!isGeneric("GenomicAnnotations2GenomicAnnotationsForPREDA")) {
   if (is.function("GenomicAnnotations2GenomicAnnotationsForPREDA")) {
       fun<-GenomicAnnotations2GenomicAnnotationsForPREDA
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotations2GenomicAnnotationsForPREDA")
   }
   setGeneric("GenomicAnnotations2GenomicAnnotationsForPREDA", fun)
}



##
## Generic GenomicAnnotationsForPREDA2GenomicAnnotations: extract the GenomicAnnotations object from the GenomicAnnotationsForPREDA object
##

if (!isGeneric("GenomicAnnotationsForPREDA2GenomicAnnotations")) {
   if (is.function("GenomicAnnotationsForPREDA2GenomicAnnotations")) {
       fun<-GenomicAnnotationsForPREDA2GenomicAnnotations
   } else {
      fun<-function(.Object) standardGeneric("GenomicAnnotationsForPREDA2GenomicAnnotations")
   }
   setGeneric("GenomicAnnotationsForPREDA2GenomicAnnotations", fun)
}


##
## Generic GenomicAnnotationsForPREDA2dataframe: extract annotations as a dataframe with probeids as rownames
##

if (!isGeneric("GenomicAnnotationsForPREDA2dataframe")) {
   if (is.function("GenomicAnnotationsForPREDA2dataframe")) {
       fun<-GenomicAnnotationsForPREDA2dataframe
   } else {
      fun<-function(.Object) standardGeneric("GenomicAnnotationsForPREDA2dataframe")
   }
   setGeneric("GenomicAnnotationsForPREDA2dataframe", fun)
}


##
## Generic GenomicAnnotationsForPREDA2PREDAResults: add PREDA results information to genomic annotatations creating a PREDAResults object
##

if (!isGeneric("GenomicAnnotationsForPREDA2PREDAResults")) {
   if (is.function("GenomicAnnotationsForPREDA2PREDAResults")) {
       fun<-GenomicAnnotationsForPREDA2PREDAResults
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsForPREDA2PREDAResults")
   }
   setGeneric("GenomicAnnotationsForPREDA2PREDAResults", fun)
}



##
## Generic StatisticsForPREDA2dataframe : extract data as a dataframe with probeids as rownames 
##

if (!isGeneric("StatisticsForPREDA2dataframe")) {
   if (is.function("StatisticsForPREDA2dataframe")) {
       fun<-StatisticsForPREDA2dataframe
   } else {
      fun<-function(.Object) standardGeneric("StatisticsForPREDA2dataframe")
   }
   setGeneric("StatisticsForPREDA2dataframe", fun)
}


##
## Generic StatisticsForPREDAFilterColumns_neg : filter statistics to remove selected analyses
##

if (!isGeneric("StatisticsForPREDAFilterColumns_neg")) {
   if (is.function("StatisticsForPREDAFilterColumns_neg")) {
       fun<-StatisticsForPREDAFilterColumns_neg
   } else {
      fun<-function(.Object, ...) standardGeneric("StatisticsForPREDAFilterColumns_neg")
   }
   setGeneric("StatisticsForPREDAFilterColumns_neg", fun)
}



##
## Generic StatisticsForPREDAFilterColumns_pos : filter statistics to keep selected analyses
##

if (!isGeneric("StatisticsForPREDAFilterColumns_pos")) {
   if (is.function("StatisticsForPREDAFilterColumns_pos")) {
       fun<-StatisticsForPREDAFilterColumns_pos
   } else {
      fun<-function(.Object, ...) standardGeneric("StatisticsForPREDAFilterColumns_pos")
   }
   setGeneric("StatisticsForPREDAFilterColumns_pos", fun)
}



##
## Generic analysesNames : get the names of the analyses in the StatisticsForPREDA object
##

if (!isGeneric("analysesNames")) {
   if (is.function("analysesNames")) {
       fun<-analysesNames
   } else {
      fun<-function(.Object) standardGeneric("analysesNames")
   }
   setGeneric("analysesNames", fun)
}


##
## Generic getStatisticByName : extract data for individual analyses using the analysis name
##

if (!isGeneric("getStatisticByName")) {
   if (is.function("getStatisticByName")) {
       fun<-getStatisticByName
   } else {
      fun<-function(.Object, ...) standardGeneric("getStatisticByName")
   }
   setGeneric("getStatisticByName", fun)
}


##
## Generic compareFunctionFromStatisticsForPREDA
##

if (!isGeneric("compareFunctionFromStatisticsForPREDA")) {
   if (is.function("compareFunctionFromStatisticsForPREDA")) {
       fun<-compareFunctionFromStatisticsForPREDA
   } else {
      fun<-function(.Object) standardGeneric("compareFunctionFromStatisticsForPREDA")
   }
   setGeneric("compareFunctionFromStatisticsForPREDA", fun)
}



##
## Generic DataForPREDA2dataframe : extract data and annotations as a dataframe with probeids as rownames 
##

if (!isGeneric("DataForPREDA2dataframe")) {
   if (is.function("DataForPREDA2dataframe")) {
       fun<-DataForPREDA2dataframe
   } else {
      fun<-function(.Object) standardGeneric("DataForPREDA2dataframe")
   }
   setGeneric("DataForPREDA2dataframe", fun)
}


##
## Generic DataForPREDA2GenomicAnnotationsForPREDA : extract a GenomicAnnotationsForPREDA object from a data DataForPREDA
##

if (!isGeneric("DataForPREDA2GenomicAnnotationsForPREDA")) {
   if (is.function("DataForPREDA2GenomicAnnotationsForPREDA")) {
       fun<-DataForPREDA2GenomicAnnotationsForPREDA
   } else {
      fun<-function(.Object) standardGeneric("DataForPREDA2GenomicAnnotationsForPREDA")
   }
   setGeneric("DataForPREDA2GenomicAnnotationsForPREDA", fun)
}



##
## Generic DataForPREDA2StatisticsForPREDA : extract a StatisticsForPREDA object from a data DataForPREDA object 
##

if (!isGeneric("DataForPREDA2StatisticsForPREDA")) {
   if (is.function("DataForPREDA2StatisticsForPREDA")) {
       fun<-DataForPREDA2StatisticsForPREDA
   } else {
      fun<-function(.Object) standardGeneric("DataForPREDA2StatisticsForPREDA")
   }
   setGeneric("DataForPREDA2StatisticsForPREDA", fun)
}


##
## Generic PREDAResults2dataframe : extact preda results statistics as a dataframe object
##

if (!isGeneric("PREDAResults2dataframe")) {
   if (is.function("PREDAResults2dataframe")) {
       fun<-PREDAResults2dataframe
   } else {
      fun<-function(.Object) standardGeneric("PREDAResults2dataframe")
   }
   setGeneric("PREDAResults2dataframe", fun)
}


##
## Generic PREDAResults2PREDADataAndResults : merge PREDAResults and input statistics to create a PREDADataAndResults object
##

if (!isGeneric("PREDAResults2PREDADataAndResults")) {
   if (is.function("PREDAResults2PREDADataAndResults")) {
       fun<-PREDAResults2PREDADataAndResults
   } else {
      fun<-function(.Object, ...) standardGeneric("PREDAResults2PREDADataAndResults")
   }
   setGeneric("PREDAResults2PREDADataAndResults", fun)
}



##
## Generic PREDAResults2GenomicRegionsSingle
##

if (!isGeneric("PREDAResults2GenomicRegionsSingle")) {
   if (is.function("PREDAResults2GenomicRegionsSingle")) {
       fun<-PREDAResults2GenomicRegionsSingle
   } else {
      fun<-function(.Object, ...) standardGeneric("PREDAResults2GenomicRegionsSingle")
   }
   setGeneric("PREDAResults2GenomicRegionsSingle", fun)
}



##
## Generic PREDAResults2GenomicRegions : identify significant genomic regions from a PREDAResults object
##

if (!isGeneric("PREDAResults2GenomicRegions")) {
   if (is.function("PREDAResults2GenomicRegions")) {
       fun<-PREDAResults2GenomicRegions
   } else {
      fun<-function(.Object, ...) standardGeneric("PREDAResults2GenomicRegions")
   }
   setGeneric("PREDAResults2GenomicRegions", fun)
}


##
## Generic PREDAResultsGetObservedFlags : extract genomic positions with significant alterations as a matrix of flags from a PREDAResults object
##

if (!isGeneric("PREDAResultsGetObservedFlags")) {
   if (is.function("PREDAResultsGetObservedFlags")) {
       fun<-PREDAResultsGetObservedFlags
   } else {
      fun<-function(.Object, ...) standardGeneric("PREDAResultsGetObservedFlags")
   }
   setGeneric("PREDAResultsGetObservedFlags", fun)
}



##
## Generic PREDADataAndResults2dataframe : extact PREDA data and results as a dataframe object
##

if (!isGeneric("PREDADataAndResults2dataframe")) {
   if (is.function("PREDADataAndResults2dataframe")) {
       fun<-PREDADataAndResults2dataframe
   } else {
      fun<-function(.Object) standardGeneric("PREDADataAndResults2dataframe")
   }
   setGeneric("PREDADataAndResults2dataframe", fun)
}


## changed to a function in version 0.99.3
# ##
# ## Generic GenomicRegions2dataframe : extract genomic regions information as a dataframe object
# ##
# 
# if (!isGeneric("GenomicRegions2dataframe")) {
#    if (is.function("GenomicRegions2dataframe")) {
#        fun<-GenomicRegions2dataframe
#    } else {
#       fun<-function(.Object) standardGeneric("GenomicRegions2dataframe")
#    }
#    setGeneric("GenomicRegions2dataframe", fun)
# }


##
## Generic GenomicRegionsChrNumber : determine the number of chromosomes with genomic regions
##

if (!isGeneric("GenomicRegionsChrNumber")) {
   if (is.function("GenomicRegionsChrNumber")) {
       fun<-GenomicRegionsChrNumber
   } else {
      fun<-function(.Object) standardGeneric("GenomicRegionsChrNumber")
   }
   setGeneric("GenomicRegionsChrNumber", fun)
}


##
## Generic GenomicRegionsNumber : determine the number of genomic regions
##

if (!isGeneric("GenomicRegionsNumber")) {
   if (is.function("GenomicRegionsNumber")) {
       fun<-GenomicRegionsNumber
   } else {
      fun<-function(.Object) standardGeneric("GenomicRegionsNumber")
   }
   setGeneric("GenomicRegionsNumber", fun)
}


##
## Generic GenomicRegionsSpan : determine the span of each genomic region
##

if (!isGeneric("GenomicRegionsSpan")) {
   if (is.function("GenomicRegionsSpan")) {
       fun<-GenomicRegionsSpan
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicRegionsSpan")
   }
   setGeneric("GenomicRegionsSpan", fun)
}


##
## Generic GenomicRegionsTotalSpan : determine the total span of genomic regions
##

if (!isGeneric("GenomicRegionsTotalSpan")) {
   if (is.function("GenomicRegionsTotalSpan")) {
       fun<-GenomicRegionsTotalSpan
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicRegionsTotalSpan")
   }
   setGeneric("GenomicRegionsTotalSpan", fun)
}



##
## Generic GenomicRegionsCreateRegionsIds : generate unique ids for GenomicRegions objects
##

if (!isGeneric("GenomicRegionsCreateRegionsIds")) {
   if (is.function("GenomicRegionsCreateRegionsIds")) {
       fun<-GenomicRegionsCreateRegionsIds
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicRegionsCreateRegionsIds")
   }
   setGeneric("GenomicRegionsCreateRegionsIds", fun)
}



##
## Generic GenomicRegionsComparison : compare two or more GenomicRegions objects to identify overlaps
##

if (!isGeneric("GenomicRegionsComparison")) {
   if (is.function("GenomicRegionsComparison")) {
       fun<-GenomicRegionsComparison
   } else {
      fun<-function(.Object1, .Object2) standardGeneric("GenomicRegionsComparison")
   }
   setGeneric("GenomicRegionsComparison", fun)
}


##
## Generic GenomicRegionsAnnotate : extract annotations from a GenomicAnnotations object for a set of regions specified as a GenomicRegions object
##

if (!isGeneric("GenomicRegionsAnnotate")) {
   if (is.function("GenomicRegionsAnnotate")) {
       fun<-GenomicRegionsAnnotate
   } else {
      fun<-function(.Object1, .Object2, ...) standardGeneric("GenomicRegionsAnnotate")
   }
   setGeneric("GenomicRegionsAnnotate", fun)
}


##
## Generic GenomicRegionsFilter_neg : filter genomic regions to remove selected chromosomes
##

if (!isGeneric("GenomicRegionsFilter_neg")) {
   if (is.function("GenomicRegionsFilter_neg")) {
       fun<-GenomicRegionsFilter_neg
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicRegionsFilter_neg")
   }
   setGeneric("GenomicRegionsFilter_neg", fun)
}


##
## Generic GenomicRegionsFilter_pos : filter genomic regions to keep selected chromosomes
##

if (!isGeneric("GenomicRegionsFilter_pos")) {
   if (is.function("GenomicRegionsFilter_pos")) {
       fun<-GenomicRegionsFilter_pos
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicRegionsFilter_pos")
   }
   setGeneric("GenomicRegionsFilter_pos", fun)
}




##
## Generic genomePlot : draw a genome plot 
##


if (!isGeneric("genomePlot")) {
   if (is.function("genomePlot")) {
       fun<-genomePlot
   } else {
      fun<-function(.Object, ...) standardGeneric("genomePlot")
   }
   setGeneric("genomePlot", fun)
}



##
## GenomicAnnotationsForPREDAGetExpectedFlags: this function is used to retrieve a matrix indicating which feature are expeted to be positive after PREDA analysis in a simulation study
##

if (!isGeneric("GenomicAnnotationsForPREDAGetExpectedFlags")) {
   if (is.function("GenomicAnnotationsForPREDAGetExpectedFlags")) {
       fun<-GenomicAnnotationsForPREDAGetExpectedFlags
   } else {
      fun<-function(.Object, ...) standardGeneric("GenomicAnnotationsForPREDAGetExpectedFlags")
   }
   setGeneric("GenomicAnnotationsForPREDAGetExpectedFlags", fun)
}





##
## computeDatasetSignature: 
##

if (!isGeneric("computeDatasetSignature")) {
   if (is.function("computeDatasetSignature")) {
       fun<-computeDatasetSignature
   } else {
      fun<-function(.Object, ...) standardGeneric("computeDatasetSignature")
   }
   setGeneric("computeDatasetSignature", fun)
}



###############################################################################
###############################################################################
##
## SETTING ALL GENERICS for other S4-classes (not defined in PREDA package)
##
###############################################################################
###############################################################################

####
#### SODEGIR_GEstatistics: generate gene expression statistics for SODEGIR procedure
####

if (!isGeneric("SODEGIR_GEstatistics")) {
   if (is.function("SODEGIR_GEstatistics")) {
       fun<-SODEGIR_GEstatistics
   } else {
      fun<-function(.Object, ...) standardGeneric("SODEGIR_GEstatistics")
   }
   setGeneric("SODEGIR_GEstatistics", fun)
}


####
#### GE_computeStatistic : function for computing a statistic for differential expression from an expression set
####

if (!isGeneric("GE_computeStatistic")) {
   if (is.function("GE_computeStatistic")) {
       fun<-GE_computeStatistic
   } else {
      fun<-function(.Object, ...) standardGeneric("GE_computeStatistic")
   }
   setGeneric("GE_computeStatistic", fun)
}





####
#### statisticsForPREDAfromEset : function for computing a statisticForPREDA object from an expression set
####

if (!isGeneric("statisticsForPREDAfromEset")) {
   if (is.function("statisticsForPREDAfromEset")) {
       fun<-statisticsForPREDAfromEset
   } else {
      fun<-function(.Object, ...) standardGeneric("statisticsForPREDAfromEset")
   }
   setGeneric("statisticsForPREDAfromEset", fun)
}






###############################################################################
###############################################################################
##
## SETTING additional GENERICS not used in the current package version
##
###############################################################################
###############################################################################


####
#### eset2GenomicAnnotations : function to extract genomic annotations from annotations library associated to an expressionset
####

if (!isGeneric("eset2GenomicAnnotations")) {
   if (is.function("eset2GenomicAnnotations")) {
       fun<-eset2GenomicAnnotations
   } else {
      fun<-function(.Object, ...) standardGeneric("eset2GenomicAnnotations")
   }
   setGeneric("eset2GenomicAnnotations", fun)
}




###
### This function can be used to trasform an expressionset into a standardized expressionset
###
###
### This function can be used to trasform a StatisticsForPREDA object into a standardized StatisticsForPREDA object
###

if (!isGeneric("GE_standardize")) {
   if (is.function("GE_standardize")) {
       fun<-GE_standardize
   } else {
      fun<-function(.Object, ...) standardGeneric("GE_standardize")
   }
   setGeneric("GE_standardize", fun)
}



####
#### GE_simulations_samplingColumns : function for shuffling columns from an expression set
####

if (!isGeneric("GE_simulations_samplingColumns")) {
   if (is.function("GE_simulations_samplingColumns")) {
       fun<-GE_simulations_samplingColumns
   } else {
      fun<-function(.Object, ...) standardGeneric("GE_simulations_samplingColumns")
   }
   setGeneric("GE_simulations_samplingColumns", fun)
}



##
## DataForPREDARandomShuffle: this function is used to randomly shuffle data from a data for PREDA object in order to remove any local dependency between data
##


if (!isGeneric("DataForPREDARandomShuffle")) {
   if (is.function("DataForPREDARandomShuffle")) {
       fun<-DataForPREDARandomShuffle
   } else {
      fun<-function(.Object, ...) standardGeneric("DataForPREDARandomShuffle")
   }
   setGeneric("DataForPREDARandomShuffle", fun)
}




##
## DataForPREDAAddEffect_single: this function is used to add a simulated effect to asingle chromsomal region on a DataForPREDA object
##

if (!isGeneric("DataForPREDAAddEffect_single")) {
   if (is.function("DataForPREDAAddEffect_single")) {
       fun<-DataForPREDAAddEffect_single
   } else {
      fun<-function(.Object, ...) standardGeneric("DataForPREDAAddEffect_single")
   }
   setGeneric("DataForPREDAAddEffect_single", fun)
}




##
## DataForPREDAAddEffects: this function is used to add a simulated effect to specified GenomicRegions on a DataForPREDA object
##

if (!isGeneric("DataForPREDAAddEffects")) {
   if (is.function("DataForPREDAAddEffects")) {
       fun<-DataForPREDAAddEffects
   } else {
      fun<-function(.Object1, .Object2, ...) standardGeneric("DataForPREDAAddEffects")
   }
   setGeneric("DataForPREDAAddEffects", fun)
}





##
## DataForPREDASimulate: this function is used to add a simulated effect to specified GenomicRegions on a DataForPREDA object and to perform additional optional shuffling of data
##

if (!isGeneric("DataForPREDASimulate")) {
   if (is.function("DataForPREDASimulate")) {
       fun<-DataForPREDASimulate
   } else {
      fun<-function(.Object1, .Object2, ...) standardGeneric("DataForPREDASimulate")
   }
   setGeneric("DataForPREDASimulate", fun)
}



##
## DataForPREDASimulationGetExpectedFlags: this function is used to retrieve a matrix indicating which feature are expeted to be positive after PREDA analysis in a simulation study
##

if (!isGeneric("DataForPREDASimulationGetExpectedFlags")) {
   if (is.function("DataForPREDASimulationGetExpectedFlags")) {
       fun<-DataForPREDASimulationGetExpectedFlags
   } else {
      fun<-function(.Object1, .Object2, ...) standardGeneric("DataForPREDASimulationGetExpectedFlags")
   }
   setGeneric("DataForPREDASimulationGetExpectedFlags", fun)
}




##
## DataForPREDAMedianCenter: this function is used to center statistics (per column) around the median
##

if (!isGeneric("DataForPREDAMedianCenter")) {
   if (is.function("DataForPREDAMedianCenter")) {
       fun<-DataForPREDAMedianCenter
   } else {
      fun<-function(.Object, ...) standardGeneric("DataForPREDAMedianCenter")
   }
   setGeneric("DataForPREDAMedianCenter", fun)
}
