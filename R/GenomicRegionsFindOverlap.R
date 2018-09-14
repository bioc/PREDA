###
### function GenomicRegionsFindOverlap 
###

GenomicRegionsFindOverlap<-function(GenomicRegions1, GenomicRegions2=NULL) {


    if ((class(GenomicRegions1) == "GenomicRegions") & (class(GenomicRegions2) == "GenomicRegions")) {
    # processa 2 genomic regions single
            if (is.null(GenomicRegions1) | is.null(GenomicRegions2)) {
            output_regions<-NULL
            } else {
            output_regions<-GenomicRegionsComparison(GenomicRegions1, GenomicRegions2)$overlapping.regions
            }

    } else if ((class(GenomicRegions1) == "list") & (is.null(GenomicRegions2))) {
    # processa 1 lista di genomic regions
            check_classes<-sapply(GenomicRegions1, class)
            if (!all(check_classes %in% c("GenomicRegions", "NULL"))) {
            stop("The list of GenomicRegions objects must contain only GenomicRegions objects")
            }
      output_regions<-GenomicRegions1[[1]]
        if (length(GenomicRegions1)>1) {
          for (index in 2:length(GenomicRegions1)) {
            if (is.null(output_regions) | is.null(GenomicRegions1[[index]])) {
            output_regions<-NULL
            } else {
            output_regions<-GenomicRegionsComparison(output_regions, GenomicRegions1[[index]])$overlapping.regions
            }
          }
        }

    } else if ((class(GenomicRegions1) == "list") & (class(GenomicRegions2) == "list")) {
    # processa 2 liste di genomic regions
            check_classes<-sapply(GenomicRegions1, class)
            if (!all(check_classes %in% c("GenomicRegions", "NULL"))) {
            stop("The list of GenomicRegions objects must contain only GenomicRegions objects")
            }
            check_classes<-sapply(GenomicRegions2, class)
            if (!all(check_classes %in% c("GenomicRegions", "NULL"))) {
            stop("The list of GenomicRegions objects must contain only GenomicRegions objects")
            }
            if (length(GenomicRegions2) != length(GenomicRegions1)) {
            stop("The lengths of the two input lists are different.")
            }

          output_regions<-list()

          for (index in 1:length(GenomicRegions1)) {
            if (is.null(GenomicRegions1[[index]]) | is.null(GenomicRegions2[[index]])) {
            output_regions<-c(output_regions, list(NULL))
            } else {
            output_regions<-c(output_regions, list(GenomicRegionsComparison(GenomicRegions1[[index]], GenomicRegions2[[index]])$overlapping.regions))
            }
          }


    } else {
    stop("please provide either two GenomicRegions objects or, altenatively, one or two lists of GenomicRegions objects as input")
    }


return(output_regions)

}





