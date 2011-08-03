### %%% HIDDEN %%% ###


genomePlot_improved<-function(genomicAnnotations,  genomicRegions=NULL, draw.blocks=TRUE, parallel.plot=TRUE, grouping=NULL, custom.labels=NULL, add.cytoband=FALSE, scale.positions=NULL, qval.threshold=0.05, use.referencePositions=FALSE, smoothStatistic.tail=NULL, smoothStatistic.threshold=NULL, region.colors=NULL) {


    # check arguments
    if (is.null(genomicRegions)) {
        if ( inherits(genomicAnnotations, what="PREDAResults") ) {
        genomicRegions<-PREDAResults2GenomicRegions(genomicAnnotations, qval.threshold=qval.threshold, use.referencePositions=use.referencePositions, smoothStatistic.tail=smoothStatistic.tail, smoothStatistic.threshold=smoothStatistic.threshold)
        }
    } else if ((class(genomicRegions)!="GenomicRegions") & (class(genomicRegions)!="list")) {
        stop("The parameter genomicRegions should contain either one single genomicRegions object or a list of genomicRegions objects")
    } else if (class(genomicRegions)=="list") {
    check_classes<-sapply(genomicRegions, class)
        if (!(length(unique(check_classes))==1 & (unique(check_classes)[1]=="GenomicRegions"))) {
        stop("The list of genomicRegions objects must contain only genomicRegions objects")
        }
    }


if (class(genomicAnnotations) != "list") {
genomicAnnotations<-list(genomicAnnotations)
}

check_classes_genomicAnnotations<-sapply(genomicAnnotations, inherits, what="GenomicAnnotations")
if (!all(check_classes_genomicAnnotations)) {
   stop("The parameter genomicAnnotations should contain either one single genomicAnnotation object or a list of genomicAnnotation objects")
}

genomicAnnotations<-lapply(genomicAnnotations, GenomicAnnotationsSortAndCleanNA)


# force formatting genomicRegions as a list
if (!is.list(genomicRegions)) {
genomicRegions<-list(genomicRegions)
}


# setting plot region.colors
if (is.null(region.colors)) {
region.colors<-rainbow(length(genomicRegions))
}

if (length(region.colors)==1) {
region.colors<-rep(region.colors, times=length(genomicRegions))
} else if (length(region.colors) < length(genomicRegions)) {
stop("The number of selected colors is lower than the number of genomic regions to be displayed")
}


# how to scale genomic positions along x axis
scale.positions.factors<-c("Mb"=1000000, "Kb"=1000, "Bp"=1)
if (is.null(scale.positions)) {
scale.positions<-1
} else if (scale.positions %in% names(scale.positions.factors)) {
scale.positions<-scale.positions.factors[scale.positions]
} else {
stop(paste("allowed values for scale.positions are:", paste(names(scale.positions.factors), collapse="; ")))
}



# ChrCopyNumber sets the number of lines to be drawn for each chromosome
if (parallel.plot) {
  if (is.null(grouping)) {
  grouping<-(1:length(genomicRegions))
  }
} else {
grouping<-rep(1, times=length(genomicRegions))
}
ChrCopyNumber<-length(unique(grouping))


if (parallel.plot) {
    if ((length(genomicAnnotations)>1) & (length(genomicAnnotations) != ChrCopyNumber)) {
    stop("The number of provided genomicAnnotations does not match the requested number of copies per chromosome")
    }
} else {
    if (length(genomicAnnotations)>1) {
    stop("When parallel.plot = FALSE the number of provided genomicAnnotations must be 1")
    }
}



# estimate chromosome length from the maximum position of available features
# only the first genomicAnnotation object is used as reference
len<-NULL
nomichromosomi<-slot(genomicAnnotations[[1]], "chromosomesLabels")
for(cc in slot(genomicAnnotations[[1]], "chromosomesNumbers")) {
current_sel<-(slot(genomicAnnotations[[1]], "chr")==cc)
len<-c(len,max((slot(genomicAnnotations[[1]], "position")[current_sel])))
}
names(len)<-nomichromosomi



# if custom labels are specified for chromosomes wider margins are required
if (!is.null(custom.labels)) {
par(mar=(c(5, 10, 4, 2) + 0.1))
}


# more space for ana dditional chromosome is required if cytobands are plotted
if (add.cytoband & length(len)==1) {
ChrCopyNumber<-(ChrCopyNumber+1)
} else if (add.cytoband & length(len)>1) {
stop("currently cytoband plot is supported only for genomic plots with one chromosome")
}


# create the plot space
plot(c(0,max(len)),c(0,((ChrCopyNumber*length(len))+1)),type="n",axes=FALSE, xlab=ifelse(add.cytoband, "", paste("Position (", names(scale.positions.factors)[which(scale.positions.factors==scale.positions)],")", sep="")),ylab=ifelse(is.null(custom.labels),"Chromosome",""),las=2)


# drawing the genome

for (shift in 0:(ChrCopyNumber-1)) {
   if (add.cytoband & shift==(ChrCopyNumber-1)) {
   ##### %%%
   ##### modify here the function for plotting cytobands
   ##### plotCytoband2(names(len), cex.axis =0.6)
   } else {
   genomic.info<-GenomicAnnotationsForPREDA2dataframe(genomicAnnotations[[(shift+1)]])[,c("chr","position","strand")]
      nullo<-apply(genomic.info,1,FUN=function(x) {
      lines(c(x["position"],x["position"]),c(((ChrCopyNumber*(which(slot(genomicAnnotations[[(shift+1)]], "chromosomesNumbers")==x["chr"])))-shift),(((ChrCopyNumber*(which(slot(genomicAnnotations[[(shift+1)]], "chromosomesNumbers")==x["chr"])))-shift)+(as.integer(x["strand"])*0.4))),col="grey")
      })
   }
}


# drawing horizontal blue lines in the middle of chromosomes
for(i in 1:length(len)){
  for (shift in 0:(ChrCopyNumber-1)) {
        if (add.cytoband & shift==(ChrCopyNumber-1)) {
        # skip
        } else {
        lines(c(1,len[i]), c(((ChrCopyNumber*i)-shift),((ChrCopyNumber*i)-shift)),col="blue")
        }
  }
}


# Chromosomes labels for y axis
if (is.null(custom.labels)) {
label<-rep(names(len), each=ChrCopyNumber)
} else {
   if (add.cytoband) {custom.labels<-c(custom.labels,"Cytobands")}
   label<-rep(rev(custom.labels), times=length(len))
}

# draw y axis
axis(2,c(1:length(label)),label,las=2)

# draw x axis
if (!add.cytoband) {
axis(1,seq(from=0,to=max(len), by=(10*scale.positions)), label=as.integer(((seq(from=0,to=max(len), by=(10*scale.positions))/scale.positions))))
}



# draw significant regions
  if (!draw.blocks) {
  stop("The current function implementation doesn\'t allow drawing significant data points as ticks.")
  } else {
  # drawing blocks
    for (group.index in 1:length(unique(grouping))) {

    gruppo<-unique(grouping)[group.index]
    genomicRegions.subset<-genomicRegions[grouping==gruppo]
    region.colors.subset<-region.colors[grouping==gruppo]

      for (element.index in 1:length(genomicRegions.subset)) {
          if (!is.null(genomicRegions.subset[[element.index]])) {
            shift<-(group.index-1) ### controlla cytoband... OK dovrebbe essere OK
            formatted.regions<-GenomicRegions2dataframe(genomicRegions.subset[[element.index]])[,c("chr","start","end")]
            colore<-region.colors.subset[element.index]
              for(i in 1:nrow(formatted.regions)) {
              polygon(c(formatted.regions[i,"start"],formatted.regions[i,"end"], formatted.regions[i,"end"], formatted.regions[i,"start"]), c(((ChrCopyNumber*(which(slot(genomicAnnotations[[1]], "chromosomesNumbers")==formatted.regions[i,"chr"])))-shift)-0.4, ((ChrCopyNumber*(which(slot(genomicAnnotations[[1]], "chromosomesNumbers")==formatted.regions[i,"chr"])))-shift)-0.4, ((ChrCopyNumber*(which(slot(genomicAnnotations[[1]], "chromosomesNumbers")==formatted.regions[i,"chr"])))-shift)+0.4, ((ChrCopyNumber*(which(slot(genomicAnnotations[[1]], "chromosomesNumbers")==formatted.regions[i,"chr"])))-shift)+0.4), col=colore)
              }
          }
      } 
    }
  }
return(TRUE)

}






