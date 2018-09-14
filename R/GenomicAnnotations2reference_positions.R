##
## GenomicAnnotations - GenomicAnnotations2reference_positions
##

setMethod("GenomicAnnotations2reference_positions", "GenomicAnnotations", function(.Object, reference_position_type=c("start", "end", "median", "strand.start", "strand.end"), withnames=TRUE) {

  if (reference_position_type %in% c("start", "end")) {
  outvector<-(slot(.Object, reference_position_type))
  } else if (reference_position_type=="median") {
  outvector<-(trunc((slot(.Object, "start")+slot(.Object, "end"))/2))
  } else if (reference_position_type=="strand.start") {
  # return actual transcription start (start if on plus strand or end if on minus strand)
  outvector<-ifelse(slot(.Object, "strand")==1, yes=slot(.Object, "start"), no=slot(.Object, "end"))
  } else if (reference_position_type=="strand.end") {
  # return actual transcription end (end if on plus strand or start if on minus strand)
  outvector<-ifelse(slot(.Object, "strand")==1, yes=slot(.Object, "end"), no=slot(.Object, "start"))
  } else {
  stop("Allowed reference_position_type values are \"start\", \"end\", \"median\", \"strand.start\" or \"strand.end\"")
  }

  # adding names to vector elements
  if (withnames) {
  names(outvector)<-slot(.Object, "ids")
  }

  return(outvector)
})


