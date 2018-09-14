
##
## method PREDAResults2GenomicRegionsSingle
##

##
## PREDAResults - PREDAResults2GenomicRegionsSingle
##

setMethod("PREDAResults2GenomicRegionsSingle", "PREDAResults", function(.Object, qval.threshold=0.05, analysisName=NULL, use.referencePositions=TRUE, smoothStatistic.tail=NULL, smoothStatistic.threshold=NULL) {
# setMethod("PREDAResults2GenomicRegionsSingle", "PREDAResults", function(.Object, qval.threshold=0.05, analysisName=NULL, use.referencePositions=TRUE, include.genes.number=FALSE, keep.annotation=NULL, smoothStatistic.tail=NULL, smoothStatistic.threshold=NULL) {

available.analysesNames<-slot(.Object, "analysesNames")
    if (length(analysisName) >1) {
    stop("The function PREDAResults2GenomicRegionsSingle should be used with one analysis at once. Please consider using PREDAResults2GenomicRegions instead")
    } else if (!(analysisName %in% available.analysesNames)) {
    stop("The selected analysesName is not among results.")
    }

    if (!is.null(smoothStatistic.threshold)) {
        if (is.null(smoothStatistic.tail)) {
        stop("Please specify the selected smooth statistic tail.")
        }
    }

    if (!is.null(smoothStatistic.tail)) {
        if (!(smoothStatistic.tail %in% c("upper", "lower"))) {
        stop("Smooth statistic tail can be only upper, lower or NULL.")
        } else if (is.null(smoothStatistic.threshold)) {
        stop("Please specify the selected smooth statistic threshold.")
        }
    }


PREDAResults_Object<-GenomicAnnotationsSortAndCleanNA(.Object, sorting_position_column=ifelse(use.referencePositions, yes="position", no="start"))

chromosomesNumbers<-slot(PREDAResults_Object, "chromosomesNumbers")
    chr<-slot(PREDAResults_Object, "chr")
    position<-slot(PREDAResults_Object, "position")
    start<-slot(PREDAResults_Object, "start")
    end<-slot(PREDAResults_Object, "end")
    qvalue<-slot(PREDAResults_Object, "qvalue")[,(available.analysesNames == analysisName)]
    smoothStatistic<-slot(PREDAResults_Object, "smoothStatistic")[,(available.analysesNames == analysisName)]

    #creo la matrice che conterrà le informazioni sui cluster
    cluster.matrix<-NULL

    for (cc in chromosomesNumbers) {
    #inizializza la flag che serve per verificare se sono dentro ad un cluster o no
    flag_cluster<-0
    #reset del contatore di cluster
    cluster_counter<-0

    chr_subset<-cbind("chr"=chr[chr==cc], "start"=start[chr==cc], "end"=end[chr==cc], "position"=position[chr==cc], "qvalue"=qvalue[chr==cc], "smoothStatistic"=smoothStatistic[chr==cc])

        for (i in 1:nrow(chr_subset)) {
            # checking cluster conditions for each gene
            if (!is.null(smoothStatistic.tail)) {
                if (smoothStatistic.tail=="upper") {
                    check.cluster<-((chr_subset[i,"qvalue"]<=qval.threshold) & (chr_subset[i,"smoothStatistic"]>=smoothStatistic.threshold))
                } else {
                    check.cluster<-((chr_subset[i,"qvalue"]<=qval.threshold) & (chr_subset[i,"smoothStatistic"]<=smoothStatistic.threshold))
                }
            } else {
            check.cluster<-(chr_subset[i,"qvalue"]<=qval.threshold)
            }
    
            if(check.cluster) {
                if (flag_cluster==0) {
                flag_cluster<-1
                cluster_counter<-cluster_counter+1
                Start_cluster<-chr_subset[i,ifelse(use.referencePositions, yes="position", no="start")]
                Chr_cluster<-chr_subset[i,"chr"]
                Start_i<-i
                } else if (flag_cluster==1) {
                #prosegue e non fa nulla
                }
            } else {
                if (flag_cluster==0) {
                #prosegue e non fa nulla
                } else if (flag_cluster==1) {
                flag_cluster<-0
                End_cluster<-chr_subset[(i-1),ifelse(use.referencePositions, yes="position", no="end")]
                End_i<-(i-1)
                riga.matrice<-c(Chr_cluster, Start_cluster, End_cluster)

                cluster.matrix<-rbind(cluster.matrix, riga.matrice)

                Start_cluster<-NULL
                Chr_cluster<-NULL
                End_cluster<-NULL
                End_i<-NULL
                Start_i<-NULL
                riga.matrice<-NULL
                }
            }
        }


        if (flag_cluster==1 & i==nrow(chr_subset)) {
        flag_cluster<-0
        End_cluster<-chr_subset[(i),ifelse(use.referencePositions, yes="position", no="end")]
        End_i<-(i)
        riga.matrice<-c(Chr_cluster, Start_cluster, End_cluster)

        cluster.matrix<-rbind(cluster.matrix,riga.matrice)

        Start_cluster<-NULL
        Chr_cluster<-NULL
        End_cluster<-NULL
        End_i<-NULL
        Start_i<-NULL
        riga.matrice<-NULL
        }
    }


    if (is.null(cluster.matrix)) {
    print(paste("No cluster found with qval.threshold =", qval.threshold,"!!"))
    return(NULL)
    } else {
    rownames(cluster.matrix)<-NULL
    cluster.matrix<-as.data.frame(cluster.matrix)
    nomicolonne<-c("chr","start","end")
    colnames(cluster.matrix)<-nomicolonne
    chr_list<-unique(cluster.matrix[,"chr"])
    chr_labels_list<-slot(.Object, "chromosomesLabels")[(slot(.Object, "chromosomesNumbers") %in% chr_list)]

    GenomicRegions_object<-GenomicRegionsFromdataframe(GenomicRegions_dataframe=cluster.matrix, chr_column="chr", start_column="start", end_column="end", chromosomesNumbers=chr_list, chromosomesLabels=chr_labels_list)


    # add annotation to genomic regions
#     if (include.genes.number) {
#     GenomicRegions_object<-GenomicRegionsAnnotate(GenomicRegions_object , .Object, getJustFeaturesNumber=TRUE)
#     }
#     if (!is.null(keep.annotation)) {
#     GenomicRegions_object<-GenomicRegionsAnnotate(GenomicRegions_object , .Object, AnnotationsHeaders=keep.annotation)
#     }


    return(GenomicRegions_object)
    }

})

