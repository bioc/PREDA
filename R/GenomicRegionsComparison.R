
##
## method GenomicRegionsComparison 
##

##
## GenomicRegions - GenomicRegionsComparison
##

setMethod("GenomicRegionsComparison", signature=c("GenomicRegions", "GenomicRegions"), function(.Object1, .Object2) {


clusters1<-as.matrix(GenomicRegions2dataframe(.Object1))
clusters2<-as.matrix(GenomicRegions2dataframe(.Object2))

  # create a matrix containing all of the boundaries
  bounds.matrix<-rbind(
  t(apply(clusters1,1,FUN=function(x) {
    return(c(x["chr"],x["start"],"START","CLUSTERS1"))
  })),
  t(apply(clusters1,1,FUN=function(x) {
    return(c(x["chr"],x["end"],"END","CLUSTERS1"))
  })),
  t(apply(clusters2,1,FUN=function(x) {
    return(c(x["chr"],x["start"],"START","CLUSTERS2"))
  })),
  t(apply(clusters2,1,FUN=function(x) {
    return(c(x["chr"],x["end"],"END","CLUSTERS2"))
  }))
  )

rownames(bounds.matrix)<-NULL
colnames(bounds.matrix)<-c("chr", "bound.position", "bound.type", "bound.group")


# initialize the objects that will contain the overlapping regions
overlapping.clusters<-NULL
difference.1.2<-NULL
difference.2.1<-NULL

elenco.cromosomi<-unique(bounds.matrix[,"chr"])



   for (cromosoma in elenco.cromosomi) {
   sel<-(bounds.matrix[,"chr"]==cromosoma)
   chromosome.subset<-bounds.matrix[sel,]
   
   # ordino il subset di dati sulla base delle posizioni dei bound tra i cluster
   ordine<-order(as.numeric(chromosome.subset[,"bound.position"]))
   chromosome.subset<-chromosome.subset[ordine,]
   # con questi due flag gestisco lo scorrimento lungo l'elenco dei
   flag.lista1<-FALSE
   flag.lista2<-FALSE
   current.bound.lista1<-0
   current.bound.lista2<-0

      for (i in 1:nrow(chromosome.subset)) {
         riga<-chromosome.subset[i,]
         new.flag<-ifelse(riga["bound.type"]=="START", TRUE, FALSE)
         new.bound<-riga["bound.position"]
         new.bound.group<-riga["bound.group"]

         if (!flag.lista1 & !flag.lista2) {
            if (!new.flag) {
            stop("ERROR: some problem occurred with cluster bounds order! Maybe clusters within uno of the two lists have overlapping regions")
            } else {
              switch(new.bound.group,
                     "CLUSTERS1" = flag.lista1<-TRUE,
                     "CLUSTERS2" = flag.lista2<-TRUE
                    )
               #setto il nuovo bound
               switch(new.bound.group,
                  "CLUSTERS1" = current.bound.lista1<-new.bound,
                  "CLUSTERS2" = current.bound.lista2<-new.bound
               )
            next
            }
         }
         if (flag.lista1 & flag.lista2) {
            if (new.flag) {
            stop("ERROR: some problem occurred with cluster bounds order! Maybe clusters within uno of the two lists have overlapping regions")
            } else {
               switch(new.bound.group,
                      "CLUSTERS1" = flag.lista1<-FALSE,
                      "CLUSTERS2" = flag.lista2<-FALSE
                     )
                 # se entrambi i flag sono accesi i current bound devono essere uguali, si potrebbe evitare questo controllo
                 # ma finchè si fa il debugging sullo script è meglio essere scrupolosi
                 if (current.bound.lista1 != current.bound.lista2) { stop("ERROR: both flags are active but boundaries are not aligned! please report this bug to the package developers!") }
                    overlapping.clusters<-rbind(overlapping.clusters, c("chr"=cromosoma, "start"=current.bound.lista1, "end"=new.bound))
                  #tengo traccia del nome del cluster
                 current.bound.lista1<-new.bound
                 current.bound.lista2<-new.bound
                 next
            }
         }
         if ((flag.lista1 & !flag.lista2) | (!flag.lista1 & flag.lista2)) {
            if (new.flag) {
               if ((flag.lista1 & new.bound.group=="CLUSTERS1") | (flag.lista2 & new.bound.group=="CLUSTERS2")) {
               stop("ERROR: some problem occurred with cluster bounds order! Maybe clusters within one of the two lists have overlapping regions")
               } else {

                  if ((new.bound.group=="CLUSTERS1" & new.bound==current.bound.lista2) | (new.bound.group=="CLUSTERS2" & new.bound==current.bound.lista1)) {
                     #accendo la flag
                     switch(new.bound.group,
                        "CLUSTERS1" = flag.lista1<-TRUE,
                        "CLUSTERS2" = flag.lista2<-TRUE
                     )
                     #setto il nuovo bound
                     switch(new.bound.group,
                        "CLUSTERS1" = current.bound.lista1<-new.bound,
                        "CLUSTERS2" = current.bound.lista2<-new.bound
                     )
                     next
                  } else {
                     if (current.bound.lista1 == current.bound.lista2) {
                        #accendo la flag
                        switch(new.bound.group,
                           "CLUSTERS1" = flag.lista1<-TRUE,
                           "CLUSTERS2" = flag.lista2<-TRUE
                        )
                        # scrivo la differenza nella matrice con le differenze
                        # visto che qui si accende il flag dove prima era spento,
                        # il cluster che ha un pezzo in più è quello diverso da new.bound.group
                        switch(new.bound.group,
                            "CLUSTERS1" = difference.2.1<-rbind(difference.2.1, c("chr"=cromosoma, "start"=(as.numeric(current.bound.lista2) + 1), "end"=(as.numeric(new.bound)-1) )),
                            "CLUSTERS2" = difference.1.2<-rbind(difference.1.2, c("chr"=cromosoma, "start"=(as.numeric(current.bound.lista1) + 1), "end"=(as.numeric(new.bound)-1) ))
                        )
                        # nuovo bound per entrambi
                        current.bound.lista1<-new.bound
                        current.bound.lista2<-new.bound
                        next
                     } else {
                        #accendo la flag
                        switch(new.bound.group,
                           "CLUSTERS1" = flag.lista1<-TRUE,
                           "CLUSTERS2" = flag.lista2<-TRUE
                        )
                        # scrivo la differenza nella matrice con le differenze
                        # visto che qui si accende il flag dove prima era spento,
                        # il cluster che ha un pezzo in più è quello diverso da new.bound.group
                           switch(new.bound.group,
                              "CLUSTERS1" = difference.2.1<-rbind(difference.2.1, c("chr"=cromosoma, "start"=current.bound.lista2, "end"=(as.numeric(new.bound)-1) )),
                              "CLUSTERS2" = difference.1.2<-rbind(difference.1.2, c("chr"=cromosoma, "start"=current.bound.lista1, "end"=(as.numeric(new.bound)-1) ))
                           )
                        # nuovo bound per entrambi
                        current.bound.lista1<-new.bound
                        current.bound.lista2<-new.bound
                        next
                     }
                  }
               }
            } else {
               if ((!flag.lista1 & new.bound.group=="CLUSTERS1") | (!flag.lista2 & new.bound.group=="CLUSTERS2")) {
               stop("ERROR: some problem occurred with cluster bounds order! Maybe clusters within uno of the two lists have overlapping regions")
               } else {
                  if ((new.bound.group=="CLUSTERS1" & new.bound==current.bound.lista1) | (new.bound.group=="CLUSTERS2" & new.bound==current.bound.lista2)) {
                  # qui siamo nel caso in cui ci siano delle regioni sovrapposte con il bound finale uguale
                     switch(new.bound.group,
                           "CLUSTERS1" = flag.lista1<-FALSE,
                           "CLUSTERS2" = flag.lista2<-FALSE
                           )
                  #tengo traccia del nome del cluster
                     next
                  } else {
                     if (current.bound.lista1 == current.bound.lista2) {
                     # in questo caso stiamo chiudendo un cluster che prima era parzialmente sovrapposto ad un altro 
                     # e dobbiamo solo fare uno shift di una base nel bound
                        switch(new.bound.group,
                              "CLUSTERS1" = flag.lista1<-FALSE,
                              "CLUSTERS2" = flag.lista2<-FALSE
                              )

                           switch(new.bound.group,
                              "CLUSTERS1" = difference.1.2<-rbind(difference.1.2, c("chr"=cromosoma, "start"=(as.numeric(current.bound.lista1) + 1), "end"=new.bound )),
                              "CLUSTERS2" = difference.2.1<-rbind(difference.2.1, c("chr"=cromosoma, "start"=(as.numeric(current.bound.lista2) + 1), "end"=new.bound ))
                           )

                        #tengo traccia del nome del cluster
                        #setto il nuovo bound
                        switch(new.bound.group,
                           "CLUSTERS1" = current.bound.lista1<-new.bound,
                           "CLUSTERS2" = current.bound.lista2<-new.bound
                        )
                        next
                     } else {
                        switch(new.bound.group,
                              "CLUSTERS1" = flag.lista1<-FALSE,
                              "CLUSTERS2" = flag.lista2<-FALSE
                              )

                           switch(new.bound.group,
                              "CLUSTERS1" = difference.1.2<-rbind(difference.1.2, c("chr"=cromosoma, "start"=current.bound.lista1, "end"=new.bound )),
                              "CLUSTERS2" = difference.2.1<-rbind(difference.2.1, c("chr"=cromosoma, "start"=current.bound.lista2, "end"=new.bound ))
                           )
                        #tengo traccia del nome del cluster
                        #setto il nuovo bound
                        switch(new.bound.group,
                           "CLUSTERS1" = current.bound.lista1<-new.bound,
                           "CLUSTERS2" = current.bound.lista2<-new.bound
                        )
                        next
                     }
                  }
               }
            }
         }
      }
   }

## recupero additional statistics on regions

if (!is.null(difference.1.2)) {
mode(difference.1.2)<-"numeric"
colnames(difference.1.2)<-c("chr", "start", "end")
difference.1.2<-GenomicRegionsFromdataframe(as.data.frame(difference.1.2),  chr_column="chr", start_column="start", end_column="end")
}
if (!is.null(difference.2.1)) {
mode(difference.2.1)<-"numeric"
colnames(difference.2.1)<-c("chr", "start", "end")
difference.2.1<-GenomicRegionsFromdataframe(as.data.frame(difference.2.1),  chr_column="chr", start_column="start", end_column="end")
}
if (!is.null(overlapping.clusters)) {
mode(overlapping.clusters)<-"numeric"
colnames(overlapping.clusters)<-c("chr", "start", "end")
overlapping.clusters<-GenomicRegionsFromdataframe(as.data.frame(overlapping.clusters),  chr_column="chr", start_column="start", end_column="end")
}



GenomicRegions1.number<-GenomicRegionsNumber(.Object1)
GenomicRegions2.number<-GenomicRegionsNumber(.Object2)
GenomicRegions1.totalspan<-GenomicRegionsTotalSpan(.Object1, unit="bp", outputNumeric=TRUE)
GenomicRegions2.totalspan<-GenomicRegionsTotalSpan(.Object2, unit="bp", outputNumeric=TRUE)

  if (is.null(overlapping.clusters)) {
  overlapping.number<-NULL
  overlapping.totalspan<-NULL
  } else {
  overlapping.number<-GenomicRegionsNumber(overlapping.clusters)
  overlapping.totalspan<-GenomicRegionsTotalSpan(overlapping.clusters, unit="bp", outputNumeric=TRUE)
  }
overlap.VS.GenomicRegions1.ratio<-(overlapping.totalspan / GenomicRegions1.totalspan)
overlap.VS.GenomicRegions2.ratio<-(overlapping.totalspan / GenomicRegions2.totalspan)


# Output results as a list
return(list("overlapping.regions"=overlapping.clusters,
"difference.1.2"=difference.1.2,
"difference.2.1"=difference.2.1,
"GenomicRegions1.number"=GenomicRegions1.number,
"GenomicRegions2.number"=GenomicRegions2.number,
"overlapping.number"=overlapping.number,
"GenomicRegions1.totalspan"=GenomicRegions1.totalspan,
"GenomicRegions2.totalspan"=GenomicRegions2.totalspan,
"overlapping.totalspan"=overlapping.totalspan,
"overlap.VS.GenomicRegions1.ratio"=overlap.VS.GenomicRegions1.ratio,
"overlap.VS.GenomicRegions2.ratio"=overlap.VS.GenomicRegions2.ratio
))


})



