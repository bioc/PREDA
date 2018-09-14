###############################################################################
###############################################################################
##
## SETTING ALL INIZIALIZE METHODS
##
###############################################################################
###############################################################################

##
## GenomicAnnotations - initialize
##

setMethod("initialize", "GenomicAnnotations", function(.Object, ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL) {
   if (any(duplicated(ids))) {
     stop("Duplicated ids are not allowed for GenomicAnnotations object. Please assign unique identifiers to the \"ids\" slot.")
   }
   if (length(ids) != length(chr)) {stop("The lengths of ids and chr are different.")}
   if (length(ids) != length(start)) {stop("The lengths of ids and start are different.")}
   if (length(ids) != length(end)) {stop("The lengths of ids and end are different.")}
   if (length(ids) != length(strand)) {stop("The lengths of ids and strand are different.")}

   if (length(chromosomesNumbers) != length(chromosomesLabels)) {stop("The lengths of chromosomeNumbers and chromosomeLabels are different.")}

if (any(!(chromosomesNumbers %in% unique(chr)))) { stop("ChromosomeNumbers includes more chromosomes than the chr vector.") }
if (any(!(names(table(chr)) %in% chromosomesNumbers))) { stop("The chr vector includes more chromosomes than ChromosomeNumbers.") }

# this is to check precence and format of optional genomic annotations
if (!(is.null(optionalAnnotations)) & length(optionalAnnotations)>0) {
    if ((is.null(optionalAnnotationsHeaders)) | length(optionalAnnotationsHeaders)==0) {
    stop("Missing optionalAnnotationsHeaders while optionalAnnotations are specified")
    }
}


if ((!is.null(optionalAnnotationsHeaders)) & length(optionalAnnotationsHeaders)>0) {
    if ((is.null(optionalAnnotations)) | length(optionalAnnotations)==0) {
    stop("Missing optionalAnnotations while headers are specified")
    } else {
        if (!(is.matrix(optionalAnnotations))) {
        optionalAnnotations<-matrix(optionalAnnotations, ncol=length(optionalAnnotationsHeaders))
        } else {
            if (length(optionalAnnotationsHeaders) != ncol(optionalAnnotations)) {stop("The lengths of optionalAnnotationsHeaders and the columns number of optionalAnnotations are different.")}
        }
        if (length(ids) != nrow(optionalAnnotations)) {
        stop("The lengths of ids and the row number of optionalAnnotations are different.")
        }
    mode(optionalAnnotations)<-"character"
    rownames(optionalAnnotations)<-ids
    optionalAnnotationsHeaders<-make.names(optionalAnnotationsHeaders)
    colnames(optionalAnnotations)<-optionalAnnotationsHeaders
    }
}


.Object@ids <- as.character(ids)
.Object@chr <- as.integer(chr)
.Object@start <- as.integer(start)
.Object@end <- as.integer(end)
.Object@strand <- strand
.Object@chromosomesNumbers <- chromosomesNumbers
.Object@chromosomesLabels <- chromosomesLabels
if (!(is.null(optionalAnnotations))) {.Object@optionalAnnotations <- optionalAnnotations}
if (!(is.null(optionalAnnotationsHeaders))) {.Object@optionalAnnotationsHeaders <- optionalAnnotationsHeaders}

.Object

})


##
## GenomicAnnotationsForPREDA - initialize
##

setMethod("initialize", "GenomicAnnotationsForPREDA", function(.Object, ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, position, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL) {

   if (length(ids) != length(position)) {stop("The lengths of ids and position are different.")}

# create a GenomicAnnotations object subsequently extended to GenomicAnnotationsForPREDA
GenomicAnnotations_data_object<-new("GenomicAnnotations",
 ids=ids,
 chr=chr,
 start=start,
 end=end,
 strand=strand,
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders
)



.Object@ids <- slot(GenomicAnnotations_data_object, "ids")
.Object@chr <- slot(GenomicAnnotations_data_object, "chr")
.Object@start <- slot(GenomicAnnotations_data_object, "start")
.Object@end <- slot(GenomicAnnotations_data_object, "end")
.Object@strand <- slot(GenomicAnnotations_data_object, "strand")
.Object@chromosomesNumbers <- slot(GenomicAnnotations_data_object, "chromosomesNumbers")
.Object@chromosomesLabels <- slot(GenomicAnnotations_data_object, "chromosomesLabels")
.Object@position <- as.integer(position)
if (!(is.null(slot(GenomicAnnotations_data_object, "optionalAnnotations")))) {.Object@optionalAnnotations <- slot(GenomicAnnotations_data_object, "optionalAnnotations")}
if (!(is.null(slot(GenomicAnnotations_data_object, "optionalAnnotationsHeaders")))) {.Object@optionalAnnotationsHeaders <- slot(GenomicAnnotations_data_object, "optionalAnnotationsHeaders")}
.Object

})


##
## StatisticsForPREDA - initialize
##

setMethod("initialize", "StatisticsForPREDA", function(.Object, ids, statistic, analysesNames, testedTail) {

   if (any(duplicated(ids))) {
     stop("Duplicated ids are not allowed for StatisticsForPREDA object. Please assign unique identifiers to the \"ids\" slot.")
   }

.Object@ids <- ids


if (length(analysesNames)<1) {
stop("Missing analysesNames.")
} else {
  if (!(is.matrix(statistic))) {
  statistic<-matrix(statistic, ncol=length(analysesNames))
  } else {
    if (length(analysesNames) != ncol(statistic)) {stop("The lengths of analysesNames and the columns number of statistic are different.")}
  }
  if (length(ids) != nrow(statistic)) {
  stop("The lengths of ids and the row number of statistic are different.")
  }
}
mode(statistic)<-"numeric"
rownames(statistic)<-ids
analysesNames<-make.names(analysesNames)
colnames(statistic)<-analysesNames

.Object@analysesNames <- analysesNames

.Object@statistic <- statistic


allowed_testedTail<-c("upper", "lower","both")
if (testedTail %in% allowed_testedTail) {
.Object@testedTail <- testedTail
} else {
stop(paste("The allowed values for testedTail are:",paste(allowed_testedTail, collapse=", ")))
}

.Object
})


##
## DataForPREDA - initialize
##

setMethod("initialize", "DataForPREDA", function(.Object, ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, position, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL, statistic, analysesNames, testedTail) {


# create a GenomicAnnotationsForPREDA object subsequently extended
GenomicAnnotationsForPREDA_data_object<-new("GenomicAnnotationsForPREDA",
 ids=ids,
 chr=chr,
 start=start,
 end=end,
 strand=strand,
 position=position,
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders
)

# create a StatisticsForPREDA object subsequently extended
StatisticsForPREDA_data_object<-new("StatisticsForPREDA",
ids=ids,
statistic=statistic,
analysesNames=analysesNames,
testedTail=testedTail
)



.Object@ids <- slot(GenomicAnnotationsForPREDA_data_object, "ids")
.Object@chr <- slot(GenomicAnnotationsForPREDA_data_object, "chr")
.Object@start <- slot(GenomicAnnotationsForPREDA_data_object, "start")
.Object@end <- slot(GenomicAnnotationsForPREDA_data_object, "end")
.Object@strand <- slot(GenomicAnnotationsForPREDA_data_object, "strand")
.Object@chromosomesNumbers <- slot(GenomicAnnotationsForPREDA_data_object, "chromosomesNumbers")
.Object@chromosomesLabels <- slot(GenomicAnnotationsForPREDA_data_object, "chromosomesLabels")
.Object@position <- slot(GenomicAnnotationsForPREDA_data_object, "position")
if (!(is.null(slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotations")))) {.Object@optionalAnnotations <- slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotations")}
if (!(is.null(slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotationsHeaders")))) {.Object@optionalAnnotationsHeaders <- slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotationsHeaders")}

.Object@analysesNames <- slot(StatisticsForPREDA_data_object, "analysesNames")
.Object@statistic <- slot(StatisticsForPREDA_data_object, "statistic") 
.Object@testedTail <- slot(StatisticsForPREDA_data_object, "testedTail") 

.Object
})


##
## PREDAResults - initialize
##

setMethod("initialize", "PREDAResults", function(.Object, ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, position, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL, analysesNames, testedTail, smoothStatistic, pvalue, qvalue) {

# create a GenomicAnnotationsForPREDA object subsequently extended
GenomicAnnotationsForPREDA_data_object<-new("GenomicAnnotationsForPREDA",
 ids=ids,
 chr=chr,
 start=start,
 end=end,
 strand=strand,
 position=position,
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders
)

.Object@ids <- slot(GenomicAnnotationsForPREDA_data_object, "ids")
.Object@chr <- slot(GenomicAnnotationsForPREDA_data_object, "chr")
.Object@start <- slot(GenomicAnnotationsForPREDA_data_object, "start")
.Object@end <- slot(GenomicAnnotationsForPREDA_data_object, "end")
.Object@strand <- slot(GenomicAnnotationsForPREDA_data_object, "strand")
.Object@chromosomesNumbers <- slot(GenomicAnnotationsForPREDA_data_object, "chromosomesNumbers")
.Object@chromosomesLabels <- slot(GenomicAnnotationsForPREDA_data_object, "chromosomesLabels")
.Object@position <- slot(GenomicAnnotationsForPREDA_data_object, "position")
if (!(is.null(slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotations")))) {.Object@optionalAnnotations <- slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotations")}
if (!(is.null(slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotationsHeaders")))) {.Object@optionalAnnotationsHeaders <- slot(GenomicAnnotationsForPREDA_data_object, "optionalAnnotationsHeaders")}


## checking PREDAResults specific parameters

if (length(analysesNames)<1) {
stop("Missing analysesNames.")
} else {

# smoothStatistic
  if (!(is.matrix(smoothStatistic))) {
  smoothStatistic<-matrix(smoothStatistic, ncol=length(analysesNames))
  } else {
    if (length(analysesNames) != ncol(smoothStatistic)) {stop("The lengths of analysesNames and the columns number of smoothStatistic are different.")}
  }
  if (length(ids) != nrow(smoothStatistic)) {
  stop("The lengths of ids and the row number of smoothStatistic are different.")
  }

# pvalue
  if (!(is.matrix(pvalue))) {
  pvalue<-matrix(pvalue, ncol=length(analysesNames))
  } else {
    if (length(analysesNames) != ncol(pvalue)) {stop("The lengths of analysesNames and the columns number of pvalue are different.")}
  }
  if (length(ids) != nrow(pvalue)) {
  stop("The lengths of ids and the row number of pvalue are different.")
  }

# qvalue
  if (!(is.matrix(qvalue))) {
  qvalue<-matrix(qvalue, ncol=length(analysesNames))
  } else {
    if (length(analysesNames) != ncol(qvalue)) {stop("The lengths of analysesNames and the columns number of qvalue are different.")}
  }
  if (length(ids) != nrow(qvalue)) {
  stop("The lengths of ids and the row number of qvalue are different.")
  }

}

analysesNames<-make.names(analysesNames)

mode(smoothStatistic)<-"numeric"
rownames(smoothStatistic)<-ids
colnames(smoothStatistic)<-analysesNames

mode(pvalue)<-"numeric"
rownames(pvalue)<-ids
colnames(pvalue)<-analysesNames

mode(qvalue)<-"numeric"
rownames(qvalue)<-ids
colnames(qvalue)<-analysesNames

.Object@analysesNames <- analysesNames

.Object@smoothStatistic <- smoothStatistic
.Object@pvalue <- pvalue
.Object@qvalue <- qvalue


allowed_testedTail<-c("upper", "lower","both")
if (testedTail %in% allowed_testedTail) {
.Object@testedTail <- testedTail
} else {
stop(paste("The allowed values for testedTail are:",paste(allowed_testedTail, collapse=", ")))
}

.Object
})


##
## PREDADataAndResults - initialize
##

setMethod("initialize", "PREDADataAndResults", function(.Object, ids, chr, start, end, strand, chromosomesNumbers, chromosomesLabels, position, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL, analysesNames, testedTail, smoothStatistic, pvalue, qvalue, statistic) {

# create a PREDADataAndResults object subsequently extended
PREDAResults_data_object<-new("PREDAResults",
 ids=ids,
 chr=chr,
 start=start,
 end=end,
 strand=strand,
 position=position,
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders,
 analysesNames=analysesNames,
 testedTail=testedTail,
 smoothStatistic=smoothStatistic,
 pvalue=pvalue,
 qvalue=qvalue
)


# create a PREDADataAndResults object subsequently extended
DataForPREDA_data_object<-new("DataForPREDA",
 ids=ids,
 chr=chr,
 start=start,
 end=end,
 strand=strand,
 position=position,
 chromosomesNumbers=chromosomesNumbers,
 chromosomesLabels=chromosomesLabels,
 optionalAnnotations=optionalAnnotations,
 optionalAnnotationsHeaders=optionalAnnotationsHeaders,
 statistic=statistic,
 analysesNames=analysesNames,
 testedTail=testedTail
)



.Object@ids <- slot(PREDAResults_data_object, "ids")
.Object@chr <- slot(PREDAResults_data_object, "chr")
.Object@start <- slot(PREDAResults_data_object, "start")
.Object@end <- slot(PREDAResults_data_object, "end")
.Object@strand <- slot(PREDAResults_data_object, "strand")
.Object@chromosomesNumbers <- slot(PREDAResults_data_object, "chromosomesNumbers")
.Object@chromosomesLabels <- slot(PREDAResults_data_object, "chromosomesLabels")
.Object@position <- slot(PREDAResults_data_object, "position")
if (!(is.null(slot(PREDAResults_data_object, "optionalAnnotations")))) {.Object@optionalAnnotations <- slot(PREDAResults_data_object, "optionalAnnotations")}
if (!(is.null(slot(PREDAResults_data_object, "optionalAnnotationsHeaders")))) {.Object@optionalAnnotationsHeaders <- slot(PREDAResults_data_object, "optionalAnnotationsHeaders")}
.Object@analysesNames <- slot(PREDAResults_data_object, "analysesNames")
.Object@smoothStatistic <- slot(PREDAResults_data_object, "smoothStatistic")
.Object@pvalue <- slot(PREDAResults_data_object, "pvalue")
.Object@qvalue <- slot(PREDAResults_data_object, "qvalue")
.Object@testedTail <- slot(PREDAResults_data_object, "testedTail")

.Object@statistic <- slot(DataForPREDA_data_object, "statistic")


.Object
})



##
## GenomicRegions - initialize
##

setMethod("initialize", "GenomicRegions", function(.Object, ids=NULL, chr, start, end, chromosomesNumbers=NULL, chromosomesLabels=NULL, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL) {
   if (!is.null(ids)) {
        if (any(duplicated(ids))) {
        stop("Duplicated ids are not allowed for GenomicAnnotations object. Please assign unique identifiers to the \"ids\" slot.")
        }
    if (length(ids) != length(chr)) {stop("The lengths of ids and chr are different.")}
    }

   if (length(chr) != length(start)) {stop("The lengths of chr and start are different.")}
   if (length(chr) != length(end)) {stop("The lengths of chr and end are different.")}

   if (length(chromosomesNumbers) != length(chromosomesLabels)) {stop("The lengths of chromosomeNumbers and chromosomeLabels are different.")}

if (!(is.null(optionalAnnotations)) & length(optionalAnnotations)>0) {
    if ((is.null(optionalAnnotationsHeaders)) | length(optionalAnnotationsHeaders)==0) {
    stop("Missing optionalAnnotationsHeaders while optionalAnnotations are specified")
    }
}


if ((!is.null(optionalAnnotationsHeaders)) & length(optionalAnnotationsHeaders)>0) {
    if ((is.null(optionalAnnotations)) | length(optionalAnnotations)==0) {
    stop("Missing optionalAnnotations while headers are specified")
    } else {
        if (!(is.matrix(optionalAnnotations))) {
        optionalAnnotations<-matrix(optionalAnnotations, ncol=length(optionalAnnotationsHeaders))
        } else {
            if (length(optionalAnnotationsHeaders) != ncol(optionalAnnotations)) {stop("The lengths of optionalAnnotationsHeaders and the columns number of optionalAnnotations are different.")}
        }
        if (length(chr) != nrow(optionalAnnotations)) {
        stop("The rows number of optionalAnnotations is different from regions number.")
        }
    mode(optionalAnnotations)<-"character"
    optionalAnnotationsHeaders<-make.names(optionalAnnotationsHeaders)
    colnames(optionalAnnotations)<-optionalAnnotationsHeaders
    }
}



if (!(is.null(ids))) {.Object@ids <- as.character(ids)}
.Object@chr <- as.integer(chr)
.Object@start <- as.integer(start)
.Object@end <- as.integer(end)
if (!(is.null(chromosomesNumbers))) {.Object@chromosomesNumbers <- chromosomesNumbers}
if (!(is.null(chromosomesLabels))) {.Object@chromosomesLabels <- chromosomesLabels}
if (!(is.null(optionalAnnotations))) {.Object@optionalAnnotations <- optionalAnnotations}
if (!(is.null(optionalAnnotationsHeaders))) {.Object@optionalAnnotationsHeaders <- optionalAnnotationsHeaders}
.Object

})

