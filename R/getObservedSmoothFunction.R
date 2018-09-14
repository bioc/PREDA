	###
### switch between available smoothing functions for observed smooth values
###


getObservedSmoothFunction<-function(smoothMethod) {

  if (smoothMethod %in% c("lokern","lokern_scaledBandwidth","lokern_scaledBandwidth_repeated")) {
  fun<-PREDA_smoothStat
  } else if (existsFunction(paste("PREDA_",smoothMethod,"Stat", sep=""))) {
  fun<-get(paste("PREDA_",smoothMethod,"Stat", sep=""))
  } else if (grepl(pattern="^runmean\\.[0-9]+$", x=smoothMethod, perl=TRUE)) {
  k_factor<-as.integer(gsub(pattern="^runmean\\.([0-9]+)$", replacement="\\1", x=smoothMethod, perl=TRUE))
  fun<-getObservedSmoothFunction_runmean(k.local=k_factor)
  } else {
  stop("The selected smoothMethod is not defined")
  }

return(fun)
}

