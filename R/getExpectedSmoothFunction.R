
getExpectedSmoothFunction<-function(smoothMethod) {

  if (smoothMethod %in% c("lokern","lokern_scaledBandwidth","lokern_scaledBandwidth_repeated")) {
  fun<-PREDA_smoothStatPerm
  } else if (existsFunction(paste("PREDA_",smoothMethod,"StatPerm",sep=""))) {
  fun<-get(paste("PREDA_",smoothMethod,"StatPerm", sep=""))
  } else if (grepl(pattern="^runmean\\.[0-9]+$", x=smoothMethod, perl=TRUE)) {
  k_factor<-as.integer(gsub(pattern="^runmean\\.([0-9]+)$", replacement="\\1", x=smoothMethod, perl=TRUE))
  fun<-getExpectedSmoothFunction_runmean(k.local=k_factor)
  } else {
  stop("The selected smoothMethod is not defined")
  }

return(fun)
}

