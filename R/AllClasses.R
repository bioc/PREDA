###############################################################################
###############################################################################
##
## DEFINING ALL CLASSES
##
###############################################################################
###############################################################################




## 
## GenomicAnnotations S4 class
## 
## This class is used to manage information about genomic features under investigation: i.e. genomic genes, SNP or others, with particular focus on the genomic coordinates of each of them.
## Other additional annotations associated to each element can be stored in a GenomicAnnotations object in the optionalAnnotations slots
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - chr: a numeric vector representing the chromosome where each ids is mapped. 
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomes X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - end: a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - strand: a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
##           User defined options will allow the conversion to this format during GenomicAnnotations objects initialization.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosomes usually not represented with a number will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic features can be managed along with genomic positions annotations.
##                        E.g. GeneSymbol or EntrezGene ids can be associated to gene related GenomicAnnotaitons objects.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "ids" slot and a number of columns equal
##                        to le thength of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: the list of names associated to optional annotations. Please avoid using spaces in annotations names.
##

setClass("GenomicAnnotations",
representation(ids="character", chr="integer", start="integer", end="integer", strand="numeric", chromosomesNumbers="numeric", chromosomesLabels="character", optionalAnnotations="matrix", optionalAnnotationsHeaders="character"),
prototype(optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL)
)






## 
## GenomicAnnotationsForPREDA S4 class
## 
## This class is equivalent to the GenomicAnnotations class but includes an additional slot specifying 
## the reference position that will be used for PREDA smoothing of data: this is included in the "position" slot.
## An unique reference position is required for PREDA analysis because this position is used for smoothing data along chromosomal coordinates.
## This reference position usaually is the start, the end, or the median posizion of each considered genomic feature, nevertheless other user defined positions could be used as well.
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - chr: a numeric vector representing the chromosome where each ids is mapped. 
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - end: a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - strand: a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
##           User defined options will allow the conversion to this format during GenomicAnnotations objects initialization.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosome usually not represented with a number must will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic features can be managed along with genomic positions annotations.
##                        E.g. GeneSymbol or EntrezGene ids can be associated to gene realted GenomicAnnotaitons objects.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "ids" slot and a number of columns equal
##                        to the length of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: character vector containing the names associated to optional annotations. Please avoid using spaces in annotations names.
##
## - position: a numeric vector of reference genomic positions that will be associated and used for each genomic feature under investigation for smoothing data during PREDA analysis.
## 

setClass("GenomicAnnotationsForPREDA", contains="GenomicAnnotations",
representation(position="integer")
)


## 
## StatisticsForPREDA S4 class
## 
## This class is used to manage the datamatrix containing statistics for PREDA analyses: i.e. the gene (or other genomic feature) centered statistics accounting for differential expression
## (or for the other type of variation under investigation)
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - statistic: a numeric matrix containing gene-centered statistics (or statistics on genomic data centered on other genomic features under investigation).
##              The statistics must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
##
## - analysesNames: a character vector of unique names associated to each column of statistic matrix.
##                  This is just a name that will be used to identify each analysis.
##
## - testedTail: a character describing what tail of the statistic distribution will be analyzed during PREDA analysis.
##               Possible values are "upper", "lower" or "both". Anyway we strongly recommend using PREDA analysis only
##               for statistics on genomic data with a symmetric distribution around zero.
##

setClass("StatisticsForPREDA",
representation(ids="character", statistic="matrix", analysesNames="character", testedTail="character")
)



## 
## DataForPREDA S4 class
## 
## This class is used to manage all of the data required as input for PREDA analysis: it is usually created by merging a GenomicAnnotationsForPREDA and a StatisticsForPREDA classes
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - chr: a numeric vector representing the chromosome where each ids is mapped. 
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - end: a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - strand: a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
##           User defined options will allow the conversion to this format during GenomicAnnotations objects initialization.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosome usually not represented with a number must will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic features can be managed along with genomic positions annotations.
##                        E.g. GeneSymbol or EntrezGene ids can be associated to gene realted GenomicAnnotaitons objects.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "ids" slot and a number of columns equal
##                        to the length of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: the list of names associated to optional annotations. Please avoid using spaces in annotations names.
##
## - position: a numeric vector of reference genomic positions that will be associated and used for each genomic feature under investigation for smoothing data during PREDA analysis.
## 
## - statistic: a numeric matrix containing gene-centered statistics (or statistics on genomic data centered on other genomic features under investigation).
##              The statistics must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
##
## - analysesNames: a character vector of unique names associated to each column of statistic matrix.
##                  This is just a name that will be used to identify each analysis.
##
## - testedTail: a character describing what tail of the statistic distribution will be analyzed during PREDA analysis.
##               Possible values are "upper", "lower" or "both". Anyway we strongly recommend using PREDA analysis only
##               for statistics on genomic data with a symmetric distribution around zero.
##

setClass("DataForPREDA",
contains=c("GenomicAnnotationsForPREDA","StatisticsForPREDA")
)




## 
## PREDAResults S4 class
## 
## this class is used to manage the basic PREDA analysis output including smoothened statistic, pvalues and qvalues.
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - chr: a numeric vector representing the chromosome where each ids is mapped. 
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - end: a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - strand: a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
##           User defined options will allow the conversion to this format during GenomicAnnotations objects initialization.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosome usually not represented with a number must will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic features can be managed along with genomic positions annotations.
##                        E.g. GeneSymbol or EntrezGene ids can be associated to gene realted GenomicAnnotaitons objects.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "ids" slot and a number of columns equal
##                        to the length of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: the list of names associated to optional annotations. Please avoid using spaces in annotations names.
##
## - position: a numeric vector of reference genomic positions that will be associated and used for each genomic feature under investigation for smoothing data during PREDA analysis.
## 
## - analysesNames: a character vector of unique names associated to each column of smoothStatistic, pvalue and qvalue matrices.
##                  This is just a name that is used to identify each analysis.
## 
## - testedTail: a character describing what tail of the statistic distribution will be analyzed during PREDA analysis.
##               Possible values are "upper", "lower" or "both". Anyway we strongly recommend using PREDA analysis only
##               for statistics on genomic data with a symmetric distribution around zero.
##
## - smoothStatistic: a numeric matrix containing smoothed observed statistics as obtained from PREDA analysis.
##              The smoothed statistics must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
## 
## - pvalue: a numeric matrix containing unadjusted gene-centered pvalues as obtained from PREDA analysis.
##              The pvalue matrix must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
## 
## - qvalue: a numeric matrix containing adjusted gene-centered pvalues as obtained from PREDA analysis:
##           i.e. usually FDR adjusted pvalues, but other multiple testing methods could be adopted as well
##           The qvalue matrix must be provided as a matrix of numeric values,
##           with a number of rows equal to the length of "ids" slot and a number of columns equal
##           to the length of "analysesNames" slot.
## 

setClass("PREDAResults",
contains="GenomicAnnotationsForPREDA", representation(analysesNames="character", testedTail="character", smoothStatistic="matrix", pvalue="matrix", qvalue="matrix")
)



## 
## PREDADataAndResults S4 class
## 
## This class is used to manage the PREDA analysis output along with corresponding input data
##
##
## SLOTS
##
## - ids: a character vector of unique identifiers for the genomic features under investigation
##
## - chr: a numeric vector representing the chromosome where each ids is mapped. 
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - end: a numeric vector of end genomic position for each genomic feature under investigation (i.e. gene, transcript, SNP or other elements).
##
## - strand: a numeric vector of strand genomic position for each genomic feature under investigation: value 1 is used for "plus" (forward) strand and value -1 for "minus" (reverse) strand.
##           User defined options will allow the conversion to this format during GenomicAnnotations objects initialization.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosome usually not represented with a number must will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes for which genomic annotations are provided in the GenomicAnnotations object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic features can be managed along with genomic positions annotations.
##                        E.g. GeneSymbol or EntrezGene ids can be associated to gene realted GenomicAnnotaitons objects.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "ids" slot and a number of columns equal
##                        to the length of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: the list of names associated to optional annotations. Please avoid using spaces in annotations names.
##
## - position: a numeric vector of reference genomic positions that will be associated and used for each genomic feature under investigation for smoothing data during PREDA analysis.
## 
## - statistic: a numeric matrix containing gene-centered statistics (or statistics on genomic data centered on other genomic features under investigation).
##              The statistics must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
##
## - analysesNames: a character vector of unique names associated to each column of statistic, smoothStatistic, pvalue and qvalue matrices.
##                  This is just a name that is used to identify each analysis.
## 
## - testedTail: a character describing what tail of the statistic distribution will be analyzed during PREDA analysis.
##               Possible values are "upper", "lower" or "both". Anyway we strongly recommend using PREDA analysis only
##               for statistics on genomic data with a symmetric distribution around zero.
##
## - smoothStatistic: a numeric matrix containing smoothed observed statistics as obtained from PREDA analysis.
##              The smoothed statistics must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
## 
## - pvalue: a numeric matrix containing unadjusted gene-centered pvalues as obtained from PREDA analysis.
##              The pvalue matrix must be provided as a matrix of numeric values,
##              with a number of rows equal to the length of "ids" slot and a number of columns equal
##              to the length of "analysesNames" slot.
## 
## - qvalue: a numeric matrix containing adjusted gene-centered pvalues as obtained from PREDA analysis:
##           i.e. usually FDR adjusted pvalues, but other multiple testing methods could be adopted as well
##           The qvalue matrix must be provided as a matrix of numeric values,
##           with a number of rows equal to the length of "ids" slot and a number of columns equal
##           to the length of "analysesNames" slot.
## 

setClass("PREDADataAndResults",
contains=c("PREDAResults", "DataForPREDA")
)


## 
## GenomicRegions S4 class
## 
## This class is used to manage genomic regions information that can be derived from PREDA analysis results
## or from other sources:e.g. relevant genomic regions from literature reports can be imported into a GenomicRegions object and compared with PREDA analysis results
##
##
## SLOTS
##
## - chr: a numeric vector representing the chromosome where each genomic region is located.
##        Please note that chromosome usually not represented with a number must will be comverted to a number as well. 
##        e.g. for Human, chromsomomes X and Y will be converted to chromsomes 23 and 24 respectively.
##        User defined options will allow this conversion during GenomicAnnotations objects initialization.
##
## - start: a numeric vector of start genomic position for each genomic region. This vector must have the same length of "chr" slot.
##
## - end: a numeric vector of end genomic position for each genomic region. This vector must have the same length of "chr" slot.
##
## - chromosomesNumbers: a numeric vector containing the list of chromosomes associated to genomic regions in the GenomicRegions object.
##                       Each chromosome is represented just once in increasing order. Please note that chromosomes usually not represented with a number will be comverted to a number as well.
##                       e.g. for Human, chromsomomees X and Y will be converted to chromsomes 23 and 24 respectively.
##
## - chromosomesLabels: a character vector containing the list of chromosomes associated to genomic regions in the GenomicRegions object.
##                      Each chromosome is represented just once in the same order as reported in chromosomesNumbers slot.
##                      This slot is actually used just to provide a label for each associated chromosome number, in case that some non numeric chromsome is used
##                      (e.g. to preserve the correspondence between chr 23 and the actual chr X in Human)
##
## - optionalAnnotations: optional annotations associated to the genomic regions can be managed along with GenomicRegions objects.
##                        E.g. the list of GeneSymbol or EntrezGene ids associated to each genomic region can be provided as optional annotation.
##                        These additional annotations are not mandatory (the default value for this slot is NULL)
##                        The additional annotations must be provided as a matrix of character,
##                        with a number of rows equal to the length of "chr", "start" and "end" slots and a number of columns equal
##                        to le thength of "optionalAnnotationsHeaders" slot.
##
## - optionalAnnotationsHeaders: the list of names associated to optional annotations. Please avoid using spaces in annotations names.
##
## - ids: a character vector of unique identifiers associated to each genomic regions. This is just an optional element of GenomicRegions objects: the default value is NULL.
##

setClass("GenomicRegions",
representation(chr="integer", start="integer", end="integer", chromosomesNumbers="numeric", chromosomesLabels="character", optionalAnnotations="matrix", optionalAnnotationsHeaders="character", ids="character"),
prototype(chromosomesNumbers=NULL, chromosomesLabels=NULL, optionalAnnotations=NULL, optionalAnnotationsHeaders=NULL, ids=NULL)
)

