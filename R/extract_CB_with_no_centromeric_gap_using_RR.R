#' extract_CB_with_no_centromeric_gap_using_RR
#'
#' extract chromatin boundaries when a chromosome has no centromeric gap
#'
#' @param chromosome xx
#' @param RR_object xx
#' @param R2DataFrame2D xx
#' @param chrID xx
#' @param chrType ....
#'
#' @return heteroChromatinBoundaries a dataframe of both centromeric chromatinboundaries, values are physical locations
#' @export

extract_CB_with_no_centromeric_gap_using_RR <- function(chromosome, RR_object, R2DataFrame2D, chrID, chrType){
    # Remember : left corresponds to dirction 1 and right to direction 2
    if(chrType == 1){   #chrID != 'X'

        # minRR = min(RR_object$regDr)
        # indexOfMinRR = match(minRR, RR_object$regDr)
        minRR =

        print(indexOfMinRR)
        swStart1 = chromosome$mb[indexOfMinRR]
        swStart2 = chromosome$mb[indexOfMinRR]

        swSize = compute_sliding_window_size(chromosome)
        print(swSize)
        heteroBoundRight = chromosome$mb[indexOfMinRR]
        heteroBoundLeft = chromosome$mb[indexOfMinRR]

        #-- left boundary (red)________________________
        found1 = FALSE
        indexOfswEnd1 = indexOfMinRR
        while(!found1 & indexOfswEnd1>1){
            # --to do : sliding window size must be a percentage of chromosome's length
            swEnd1 = swStart1-swSize # unit here is Mb .. this value may not belong to chromosome$mb, so we find the closest using the next line
            indexOfswEnd1 = which.min(abs(chromosome$mb-swEnd1))
            indexOfswStart1 = which.min(abs(chromosome$mb-swStart1)) # we also use this formula because its value will be updated with the value of indexOfswEnd1 which does not necessarly belong to chromosme$mb
            r2 = R2DataFrame2D$R2Vect_dir1[indexOfswEnd1 : indexOfswStart1] # vector of r2 values according to the sliding window elements
            mb = chromosome$mb[indexOfswEnd1 : indexOfswStart1]
            sw1 = data.frame(r2, mb)
            if(nrow(sw1)> 1){ # do this only if there are at least 2 points
                growthRates1 = c() # les pentes locales entre chaque 2 points consécutives
                for(k in 1:(nrow(sw1)-1)){ # for all points included in current sliding window (sw)
                    if((sw1$mb[k+1]-sw1$mb[k]) != 0){
                        growthRates1 = c(growthRates1, (sw1$r2[k+1]-sw1$r2[k]) / (sw1$mb[k+1]-sw1$mb[k]))
                    }
                }
                meanGrowthRate1 = mean(growthRates1)
                if(meanGrowthRate1 > 0){ # the window stops here / now
                    # IndexOfHeteroBoundLeft = indexOfswStart1  #max of this sw pts
                    heteroBoundLeft = chromosome$mb[indexOfswEnd1]
                    found1 = TRUE
                }else{
                    swStart1 = swEnd1
                }
            }else{
                swStart1 = swEnd1
            }
        }
        indexHBleft = indexOfswEnd1
        print(indexHBleft)
        # right boundary (purple)________________________
        found2 = FALSE
        indexOfswEnd2 = indexOfMinRR
        while(!found2 & indexOfswEnd2<nrow(R2DataFrame2D)){
            swEnd2 = swStart2+swSize # unit here is Mb .. this value may not belong to chromosome$mb, so we find the closest
            indexOfswStart2 = which.min(abs(chromosome$mb-swStart2))
            indexOfswEnd2 = which.min(abs(chromosome$mb-swEnd2))
            r2 = R2DataFrame2D$R2Vect_dir2[indexOfswStart2 : indexOfswEnd2]
            mb = chromosome$mb[indexOfswStart2 : indexOfswEnd2]
            sw2 = data.frame(r2, mb)
            if (nrow(sw2)> 1){
                growthRates2 = c()
                for(m in 1:(nrow(sw2)-1)){ # for all points included in current sliding window (sw)
                    if((sw2$mb[m+1]-sw2$mb[m]) != 0){
                        growthRates2 = c(growthRates2, (sw2$r2[m+1]-sw2$r2[m]) / (sw2$mb[m+1]-sw2$mb[m]))
                    }
                }
                meanGrowthRate2 = mean(growthRates2)
                if(meanGrowthRate2 < 0){
                    # IndexOfHeteroBoundRight = indexOfswStart2 #max of this sw pts
                    heteroBoundRight = chromosome$mb[indexOfswEnd2]
                    found2 = TRUE
                }else{
                    swStart2 = swEnd2
                }
            }else{
                swStart2 = swEnd2
            }
        }
        indexHBright = indexOfswEnd2
        print(indexHBright)
    }else{ # works only for droso's one-arm chromosome X, later find a global way to apply this on other genomes
        minRR = min(RR_object$regDr)
        indexOfMinRR = match(minRR, RR_object$regDr)
        IndexOfHeteroBound = indexOfMinRR
        swStart = chromosome$mb[indexOfMinRR]
        swSize = compute_sliding_window_size(chromosome)
        found = FALSE
        indexOfswEnd = IndexOfHeteroBound
        while(!found & indexOfswEnd>1){
            swEnd = swStart-swSize # unit here is Mb .. this value may not belong to chromosome$mb, so we find the closest using the next line
            indexOfswEnd = which.min(abs(chromosome$mb-swEnd))
            indexOfswStart = which.min(abs(chromosome$mb-swStart)) # we also use this formula because its value will be updated with the value of indexOfswEnd1 which does not necessarly belong to chromosme$mb
            r2 = R2DataFrame2D$R2Vect_dir2[indexOfswEnd : indexOfswStart]
            mb = chromosome$mb[indexOfswEnd : indexOfswStart]
            sw = data.frame(r2, mb)
            if(nrow(sw)> 1){ # do this only if there are at least 2 points
                growthRates = c()
                for(k in 1:(nrow(sw)-1)){ # for all points included in current sliding window (sw)
                    if((sw$mb[k+1]-sw$mb[k]) != 0){
                        growthRates = c(growthRates, (sw$r2[k+1]-sw$r2[k]) / (sw$mb[k+1]-sw$mb[k]))
                    }
                }
                meanGrowthRate = mean(growthRates)
                if(meanGrowthRate > 0){
                    # IndexOfHeteroBound = indexOfswStart  #max of this sw pts
                    heteroBoundRight = chromosome$mb[indexOfswEnd]  # initial value
                    i = nrow(sw)
                    while(!found & i>1){
                        if((sw$mb[i]-sw$mb[i-1]) != 0){
                            a = (sw$r2[i]-sw$r2[i-1]) / (sw$mb[i]-sw$mb[i-1])
                            if(a >= 0){ #--------------------it was strictly < then it wouldn't work for chr X R6 so I changed it to <= so it worked but needs to be verified with all other chrms and versions
                                heteroBoundRight = sw$mb[i]  # get the index here n not the mb value
                                found = TRUE
                            }
                        }
                        i=i-1
                    }
                }else{
                    swStart = swEnd
                }
            }else{
                swStart = swEnd
            }
        }
        indexHBright = match(heteroBoundRight  , chromosome$mb)
        heteroBoundLeft = 0
        indexHBleft = 0
    }

    heteroChromatinBoundaries = data.frame( heteroBoundLeft, indexHBleft, heteroBoundRight, indexHBright, swSize)
    heteroChromatinBoundaries_to_print = data.frame( heteroBoundLeft, heteroBoundRight, swSize)
    cat("\n")
    print(heteroChromatinBoundaries_to_print)
    return(heteroChromatinBoundaries)
}
