#' extract_CB_with_no_centromeric_gap
#'
#' extract chromatin boundaries when a chromosome has no centromeric gap
#'
#' @param chromosome xx
#' @param RR_object xx
#' @param R2DataFrame2D xx
#' @param chrID xx
#' @param chrType ....
#' @param minRR_object xxx
#'
#' @return heteroChromatinBoundaries a dataframe of both centromeric chromatinboundaries, values are physical locations
#' @export

extract_CB_with_no_centromeric_gap <- function(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object){
  # Remember : left corresponds to dirction 1 and right to direction 2
  if(chrType == 1){   #whole chromosome : works on metacentric chromosomes =======================================================================================

    minRR = minRR_object$minRRpoly # min(RR_vector)
    indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)

    # print(indexOfMinRR)
    swStart1 = chromosome$mb[indexOfMinRR]
    swStart2 = chromosome$mb[indexOfMinRR]
    # print("--1---swStart1 and swStart2")
    # print(swStart1)
    # print(swStart2)
    swSize = compute_sliding_window_size(chromosome)
    # print("swSize is ")
    # print(swSize)
    heteroBoundRight = chromosome$mb[indexOfMinRR]
    heteroBoundLeft = chromosome$mb[indexOfMinRR]
    # print(c("initial HCB ", heteroBoundRight, heteroBoundRight))

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
            # print("from if")
          }
            # print("k")
            # print(k)
        }
        meanGrowthRate1 = mean(growthRates1)
        print(c("extract_CB_with_no_centromeric_gap - chrType meta - just before if(meanGrowthRate1 > 0) => meanGrowthRate1 =", meanGrowthRate1))
        # print("meanGrowthRate1")
        # print(meanGrowthRate1)
        if(meanGrowthRate1 > 0){ # the window stops here / now
          # IndexOfHeteroBoundLeft = indexOfswStart1  #max of this sw pts
          heteroBoundLeft = chromosome$mb[indexOfswEnd1]
          found1 = TRUE
        }else{
          swStart1 = swEnd1
          # print("from else1")
        }
      }else{
        swStart1 = swEnd1
        # print("from else2")
      }
      # print("--2--swStart1 and swStart2")
      # print(swStart1)
      # print(swStart2)
    }
    indexHBleft = indexOfswEnd1
# print(indexHBleft)
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
        print(c("extract_CB_with_no_centromeric_gap - chrType meta - just before if(meanGrowthRate2 > 0) => meanGrowthRate2 =", meanGrowthRate2))
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

    heteroChromatinBoundaries = data.frame( heteroBoundLeft, indexHBleft, heteroBoundRight, indexHBright, swSize) ## final result to return
    heteroChromatinBoundaries_to_print = data.frame( heteroBoundLeft, heteroBoundRight, swSize)

  }else{  #chromosomal arm : works on telocentric chromosomes =================================================================================
    # print(RR_object$regDr)
    minRR = minRR_object$minRRpoly # min(RR_vector)
    indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)

    IndexOfHeteroBound = indexOfMinRR
    swStart = chromosome$mb[indexOfMinRR]
    swSize = compute_sliding_window_size(chromosome)
    found = FALSE
    indexOfswEnd = IndexOfHeteroBound
    print(c("minRR = ", minRR, "indexOfMinRR = ", indexOfMinRR,  "indexOfswEnd = ", indexOfswEnd))

    if(indexOfMinRR == nrow(chromosome)){ # this is a left ahromosomal arm (exp : 2L) => left boundary (red)
        while(!found & indexOfswEnd >= nrow(chromosome)/2){  ## before was & indexOfswEnd>1
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
                print(c("extract_CB_with_no_centromeric_gap -left arm - just before if(meanGrowthRate > 0)", meanGrowthRate))
                if(meanGrowthRate > 0){
                  # IndexOfHeteroBound = indexOfswStart  #max of this sw pts
                  heteroBoundLeft = chromosome$mb[indexOfswEnd]  # initial value
                  i = nrow(sw)
                  while(!found & i>1){
                    if((sw$mb[i]-sw$mb[i-1]) != 0){
                      a = (sw$r2[i]-sw$r2[i-1]) / (sw$mb[i]-sw$mb[i-1])
                      if(a > 0){
                        heteroBoundLeft = sw$mb[i]  # get the index here n not the mb value
                        found = TRUE
                      }
                    }
                    i=i-1
                  }
                }else{
                  swStart = swEnd
                  print(c("from 1st else : swStart = swEnd ", swStart))
                }
              }else{
                swStart = swEnd
                print(c("from 2nd else : swStart = swEnd", swStart))
              }
        }
        if(found){
            indexHBleft = match(heteroBoundLeft, chromosome$mb)
            heteroBoundArm = heteroBoundLeft
            indexHBArm = indexHBleft
        }else{ #  (nrow(chromosome)/2 is reached while !found)
            heteroBoundArm = 'na'
            indexHBArm = 'na'
        }

    }else if(indexOfMinRR == 1){ # this is a right chromosomal arm (exp : 2R) => right boundary (purple)
        while(!found & indexOfswEnd <= nrow(chromosome)/2){
            swEnd = swStart+swSize # unit here is Mb .. this value may not belong to chromosome$mb, so we find the closest using the next line
            print(c("swEnd = swStart+swSize", swEnd, swStart, swSize))
            indexOfswEnd = which.min(abs(chromosome$mb-swEnd))
            indexOfswStart = which.min(abs(chromosome$mb-swStart)) # we also use this formula because its value will be updated with the value of indexOfswEnd1 which does not necessarly belong to chromosme$mb
            print(c("indexOfswEnd = ", indexOfswEnd, "indexOfswStart = ", indexOfswStart))
            r2 = R2DataFrame2D$R2Vect_dir1[indexOfswStart : indexOfswEnd]
            mb = chromosome$mb[indexOfswStart : indexOfswEnd]
            sw = data.frame(r2, mb)
            if(nrow(sw)> 1){ # do this only if there are at least 2 points
                growthRates = c()
                for(k in 1:(nrow(sw)-1)){ # for all points included in current sliding window (sw)
                    if((sw$mb[k+1]-sw$mb[k]) != 0){
                        growthRates = c(growthRates, (sw$r2[k+1]-sw$r2[k]) / (sw$mb[k+1]-sw$mb[k]))
                    }
                }
                meanGrowthRate = mean(growthRates)
                print("extract_CB_with_no_centromeric_gap -right arm - just before if(meanGrowthRate > 0)")
                print(meanGrowthRate)
                if(meanGrowthRate > 0){
                    # IndexOfHeteroBound = indexOfswStart  #max of this sw pts
                    heteroBoundRight = chromosome$mb[indexOfswEnd]  # initial value
                    i = nrow(sw)
                    while(!found & i>1){
                        if((sw$mb[i]-sw$mb[i-1]) != 0){
                            a = (sw$r2[i]-sw$r2[i-1]) / (sw$mb[i]-sw$mb[i-1])
                            if(a > 0){ #--------------------it was strictly < then it wouldn't work for chr X R6 so I changed it to <= so it worked but needs to be verified with all other chrms and versions
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
        if(found){
            indexHBright = match(heteroBoundRight, chromosome$mb)
            heteroBoundArm = heteroBoundRight
            indexHBArm = indexHBright
        }else{ #  (nrow(chromosome)/2 is reached while !found)
            heteroBoundArm = 'na'
            indexHBArm = 'na'
        }
    }else{ # (indexOfMinRR != 1 and != nrow(chromosome) )
      heteroBoundArm = 'na'
      indexHBArm = 'na'
    }

    heteroChromatinBoundaries = data.frame(heteroBoundArm, indexHBArm, swSize) ## final result to return
    heteroChromatinBoundaries_to_print = data.frame(heteroBoundArm, swSize)
  }



  print(heteroChromatinBoundaries_to_print)

  return(heteroChromatinBoundaries)
}
