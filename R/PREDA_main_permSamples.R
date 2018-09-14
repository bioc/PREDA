###
### This function is very similar to the standard PREDA function but it is based on a different permutations schema: samples are permuted and a statistic for evaluating differences between two groups is computed
###


#########################################################################
####  FUNCTION CODE BEGIN

PREDA_main_permSamples<-function(inputDataForPREDA, sampleClasses, statisticType="tstatistic", outputGenomicAnnotationsForPREDA=NULL, nperms=100, verbose=FALSE, parallelComputations=FALSE, multTestCorrection="qvalue", blocksize=10, smoothMethod="lokern", force=FALSE, lokern_scaledBandwidthFactor=1, logged=TRUE) {

    if (class(inputDataForPREDA) != "DataForPREDA") {
    stop("inputDataForPREDA is not an object of class DataForPREDA.")
    }

    if (!(smoothMethod %in% c("lokern", "quantsmooth", "lokern_scaledBandwidth", "lokern_scaledBandwidth_repeated"))) {
    stop(paste("smoothMethod allowed options are", paste(c("lokern", "quantsmooth"),collapse=" & ")))
    }

# removing invalid annotations and sorting annotations per chromosome
inputDataForPREDA<-GenomicAnnotationsSortAndCleanNA(inputDataForPREDA)

   # when not specified, the genomic annotations for the ouput are equal to the genomic annotations for input
   if (is.null(outputGenomicAnnotationsForPREDA)) {
   outputGenomicAnnotationsForPREDA<-DataForPREDA2GenomicAnnotationsForPREDA(inputDataForPREDA)
   JoinResultsWithInputStats<-TRUE
   } else if (class(outputGenomicAnnotationsForPREDA) != "GenomicAnnotationsForPREDA") {
   stop("outputGenomicAnnotationsForPREDA is not an object of class GenomicAnnotationsForPREDA.")
   } else {
   outputGenomicAnnotationsForPREDA<-GenomicAnnotationsSortAndCleanNA(outputGenomicAnnotationsForPREDA)
   JoinResultsWithInputStats<-FALSE
   }

# checking the multTestCorrection value
if (length(multTestCorrection)>1) {
stop("Only one type of multiple testing correction is allowed!")
} else if (!(multTestCorrection %in% c("qvalue", "fdr", "none"))) {
stop("Only \"qvalue\", \"fdr\" or \"none\" are allowed as multiple testing corrections.")
}



# extracting annotations for input data
input_positions<-slot(inputDataForPREDA, "position")
input_chr<-slot(inputDataForPREDA, "chr")

# check if data are appropriate for quantsmoothing
if ((smoothMethod == "quantsmooth") & (max(table(input_chr))>2000) ) {
  if (force) {
  warning("We recommend using quantsmooth method only for low density genomic data due to the low computational performance with long data vectors.")
  } else {
  stop("We recommend using quantsmooth method only for low density genomic data due to the low computational performance with long data vectors.")
  }
}



# extracting annotations for output data
output_positions<-slot(outputGenomicAnnotationsForPREDA, "position")
output_chr<-slot(outputGenomicAnnotationsForPREDA, "chr")
output_chr_unique<-unique(output_chr)

# check if input chromosomes include output chromosomes
if (any(!(output_chr_unique %in% unique(input_chr)))) {
missing_chrs<-output_chr_unique[(!(output_chr_unique %in% unique(input_chr)))]
stop(paste("Output required chromosomes are more than input chromosomes. Missing chromosomes:", paste(missing_chrs, collapse="; ")))
}

# check if input chromosomes have enough data points
count_point_per_chr<-table(input_chr)
if (any(count_point_per_chr[as.character(output_chr_unique)] < 10)) {
stop("At least 10 statistics per output chromosome are required!!")
} else if (any(count_point_per_chr[as.character(output_chr_unique)] < 30)) {
warning("At least 30 statistics per output chromosome are highly recommended")
}


# obtaining the proper function for comparing observed and expected results: it depends on the tail of statistics tha we are examining
compareWithObservedSmoothFunction<-compareFunctionFromStatisticsForPREDA(inputDataForPREDA)

# switching between smooth methods
observedSmoothFunction<-getObservedSmoothFunction(smoothMethod=smoothMethod)


# creating matrices to manage the results
smoothStatsMatrix<-NULL
pvalueMatrix<-NULL
qvalueMatrix<-NULL

    if (verbose) {
    print(paste("Processing samples with",statisticType))
    }

    # compute statistic from data
    observed_datamatrix<-slot(inputDataForPREDA, "statistic")
    current_input_statistics<-GE_computeStatistic_onMatrix(datamatrix=observed_datamatrix, classFactor=sampleClasses, statisticType=statisticType, logged=logged)

    # get bandwidth if required
    if (smoothMethod %in% c("lokern_scaledBandwidth", "lokern_scaledBandwidth_repeated")) {
        observedSmoothStat_badwidth<-observedSmoothFunction(input_stat=current_input_statistics, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=TRUE)
        scaledBandwidth<-trunc(observedSmoothStat_badwidth/lokern_scaledBandwidthFactor)
    } else {
        scaledBandwidth<-NULL
    }

    # the bandwith used for the expected smooth estimation must be equal to NULL for every smoothMethod different from lokern_scaledBandwidth
    if (smoothMethod=="lokern_scaledBandwidth_repeated") {
        scaledBandwidth_permutations<-NULL
    } else {
        scaledBandwidth_permutations<-scaledBandwidth
    }

    # get observed smooth statistic using the function defined on the fly
    observedSmoothStat<-observedSmoothFunction(input_stat=current_input_statistics, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=FALSE, bandwidth=scaledBandwidth)

    pvalue<-vector(mode="numeric", length=length(output_positions))

        if (parallelComputations) {
        require(Rmpi)
        require(rsprng)
        todrop<-mpi.spawn.Rslaves()

        # sending data to slaves
        mpi.bcast.Robj2slave(obj = PREDA_smoothStat)
        mpi.bcast.Robj2slave(obj = PREDA_quantsmoothStat)
        mpi.bcast.Robj2slave(obj = observedSmoothFunction)
        mpi.bcast.Robj2slave(obj = GE_simulations_samplingColumns_onMatrix)
        mpi.bcast.Robj2slave(obj = GE_computeStatistic_onMatrix)
        mpi.bcast.Robj2slave(obj = observed_datamatrix)
        mpi.bcast.Robj2slave(obj = sampleClasses)
        mpi.bcast.Robj2slave(obj = input_chr)
        mpi.bcast.Robj2slave(obj = input_positions)
        mpi.bcast.Robj2slave(obj = output_chr)
        mpi.bcast.Robj2slave(obj = output_positions)
        mpi.bcast.Robj2slave(obj = output_chr_unique)
        mpi.bcast.Robj2slave(obj = smoothMethod)
        mpi.bcast.Robj2slave(obj = scaledBandwidth_permutations)
        mpi.bcast.Robj2slave(obj = lokern_scaledBandwidthFactor)
        mpi.bcast.Robj2slave(obj = compareWithObservedSmoothFunction)
        mpi.bcast.Robj2slave(obj = observedSmoothStat)


        # initializing random number generators
        todrop<-mpi.remote.exec(require(rsprng))
        todrop<-mpi.remote.exec(init.sprng(nstream=(mpi.comm.size()-1), streamno=(mpi.comm.rank()-1)))


            Listen_on_slaves<-function() {
            permutation_to_run<-mpi.recv.Robj(source=0, tag=mpi.any.tag())
            input_info<-mpi.get.sourcetag()
            current_tag<-input_info[2]
            # three types of tags are used to control program flow
            # 1<-initialization step
            # 2<-running phase
            # 3<-finalizing step
            pvalue_local<-vector(mode="numeric", length=length(output_positions))
    
                while (current_tag != 3) {
                    for (perm in (1:permutation_to_run)) {
                      expected_datamatrix<-GE_simulations_samplingColumns_onMatrix(datamatrix=observed_datamatrix, classFactor=sampleClasses)
                      permutedStat<-GE_computeStatistic_onMatrix(datamatrix=expected_datamatrix, classFactor=sampleClasses, statisticType=statisticType, logged=logged)
                        if (smoothMethod=="lokern_scaledBandwidth_repeated") {
                          expectedSmoothStat_badwidth<-observedSmoothFunction(input_stat=permutedStat, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=TRUE)
                          scaledBandwidth_permutations<-trunc(expectedSmoothStat_badwidth/lokern_scaledBandwidthFactor)
                        }
                      permutedSmoothStat<-observedSmoothFunction(input_stat=permutedStat, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=FALSE, bandwidth=scaledBandwidth_permutations)
                      pvalue_local<-(pvalue_local+compareWithObservedSmoothFunction(observed=observedSmoothStat, permuted=permutedSmoothStat))
                    }
                mpi.send.Robj(obj=perm, dest=0, tag=2)
                permutation_to_run<-mpi.recv.Robj(source=0, tag=mpi.any.tag())
                task_info <- mpi.get.sourcetag()
                current_tag <- task_info[2]
                }
                # Send a message to master indicating the the slave has finished doing tasks.
                mpi.send.Robj(obj=pvalue_local,dest=0, tag=3)
                return(TRUE)
            }

        mpi.bcast.Robj2slave(obj = Listen_on_slaves)


        #initializing counters
        counter_total_permutations_sent<-0
        n_slaves<-(mpi.comm.size()-1)
        counter_total_permutations_done<-0
        closed_slave<-0

            if (verbose) {
            pb<-txtProgressBar(min = 0, max = (nperms*2), initial = 0, char = "=", style = 3)
            setTxtProgressBar(pb, value=0)
            }


            # igniting slave processes
            for (index in 1:(n_slaves)) {
            permutation_to_run<-min(blocksize, (nperms-counter_total_permutations_sent))
            init_tag<-ifelse(permutation_to_run>0,yes=1, no=3)
            mpi.isend.Robj(obj=permutation_to_run, dest=index, tag=init_tag)
            counter_total_permutations_sent<-(counter_total_permutations_sent+permutation_to_run)
                if (verbose) {
                setTxtProgressBar(pb, value=(counter_total_permutations_done+counter_total_permutations_sent))
                }
            }

            mpi.bcast.cmd({
            Listen_on_slaves()
            })


            # controlling slaves runs from master
            while ((counter_total_permutations_done < nperms) | (closed_slave < n_slaves)) {

            message <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
            message_info <- mpi.get.sourcetag()
            slave_id <- message_info[1]
            tag <- message_info[2]


                if (tag==2) {
                # message contains permutations number
                counter_total_permutations_done<-(counter_total_permutations_done+message)
                    permutation_to_run<-min(blocksize, (nperms-counter_total_permutations_sent))
                    next_tag<-ifelse(permutation_to_run>0,yes=2, no=3)
                    mpi.isend.Robj(obj=permutation_to_run, dest=slave_id, tag=next_tag)
                    counter_total_permutations_sent<-(counter_total_permutations_sent+permutation_to_run)
                    if (verbose) {
                    setTxtProgressBar(pb, value=(counter_total_permutations_done+counter_total_permutations_sent))
                    }
                } else if (tag==3) {
                # message contains pvalues from slaves
                pvalue<-(pvalue+message)
                closed_slave<-(closed_slave+1)
                } else {
                stop("Unexpected message tag from slave")
                }
            }

            if (verbose) {
            close(con=pb)
            }


            # finalizing cluster and random number generator
            todrop<-mpi.remote.exec({free.sprng()})
            #mpi.finalize()
            todrop<-mpi.close.Rslaves()


        } else {

        # the progress bar can be displaied only with serial version of the function
            if (verbose) {
            pb<-txtProgressBar(min = 0, max = nperms, initial = 0, char = "=", style = 3)
            setTxtProgressBar(pb, value=0)
            }

            # runnine permutations with serial computations
            for (perm in (1:nperms)) {
                #update progress bar
                if (verbose) {
                setTxtProgressBar(pb, value=perm)
                }

              # permute and smoothing using the function defined on the fly
              expected_datamatrix<-GE_simulations_samplingColumns_onMatrix(datamatrix=observed_datamatrix, classFactor=sampleClasses)
              permutedStat<-GE_computeStatistic_onMatrix(datamatrix=expected_datamatrix, classFactor=sampleClasses, statisticType=statisticType, logged=logged)
                if (smoothMethod=="lokern_scaledBandwidth_repeated") {
                  expectedSmoothStat_badwidth<-observedSmoothFunction(input_stat=permutedStat, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=TRUE)
                  scaledBandwidth_permutations<-trunc(expectedSmoothStat_badwidth/lokern_scaledBandwidthFactor)
                }
              permutedSmoothStat<-observedSmoothFunction(input_stat=permutedStat, input_chr=input_chr, input_positions=input_positions, output_chr=output_chr, output_positions=output_positions, output_chr_unique=output_chr_unique, returnBandwidth=FALSE, bandwidth=scaledBandwidth_permutations)


              # the compareWithObservedSmoothFunction is generated in the initial steps of this function on the basis of the selected tail of data that is analzed 
              pvalue<-(pvalue+compareWithObservedSmoothFunction(observed=observedSmoothStat, permuted=permutedSmoothStat))

            }

            if (verbose) {
            close(con=pb)
            }
        }

    # computing pvalues
    pvalue<-((nperms-pvalue)/nperms)

    # computing qvalue
    current_qvalue<-PREDA_multTestCorrection(pvalues=pvalue, method=multTestCorrection)

    # collecting results from each analysis
    smoothStatsMatrix<-cbind(smoothStatsMatrix, observedSmoothStat)
    pvalueMatrix<-cbind(pvalueMatrix, pvalue)
    qvalueMatrix<-cbind(qvalueMatrix, current_qvalue)

#    }



#Formatting and returning results

PREDAResults_object<-GenomicAnnotationsForPREDA2PREDAResults(outputGenomicAnnotationsForPREDA, analysesNames=statisticType, testedTail=slot(inputDataForPREDA, "testedTail"), smoothStatistic=smoothStatsMatrix, pvalue=pvalueMatrix, qvalue=qvalueMatrix)

# the results include input statistics if the output annotations are derived from input DataForPREDA object
    if (JoinResultsWithInputStats) {
    PREDADataAndResults_object<-PREDAResults2PREDADataAndResults(PREDAResults_object, statistic=current_input_statistics)
    return(PREDADataAndResults_object)
    } else {
    return(PREDAResults_object)
    }

}




### FUNCTION CODE END
################################################################################
################################################################################



