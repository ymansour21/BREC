#' compute_cumulated_R_squared_2directions
#'
#' compute cumulated R-squared vectors in two directions : starting from the beginning then the end of chromosome
#'
#'
#' @param chromosome  xx
#'
#' @import stats
#' @importFrom rsq rsq
#' @return R2DataFrame2D
#' @export

compute_cumulated_R_squared_2directions <- function(chromosome){ # it's not the R2 which is cumulated, instead, it's the data points used for the computation of R2

    chrData = data.frame(chrCm = chromosome$cm, chrPos = chromosome$mb)

    # R-sequared for the 1st direction : forward
    R2Vect_dir1 = c()
    for(i in 1:nrow(chrData)){
        localMB_dir1 = chrData$chrPos[1:i]
        localCM_dir1 = chrData$chrCm[1:i]

        # if(i <= 5){
        #     print(c("i = ", i))
        #     print(data.frame(localMB_dir1, localCM_dir1))
        # }

        if ((chrData$chrCm[i]!= 0) & (length(localMB_dir1) > 4) ){ # less than 3 points doesn't give correct cubic polynomial
            # localPolynomial_dir1 = summary(lm(localCM_dir1 ~ poly(localMB_dir1, 3, raw = T)))
            # R2Vect_dir1 = c(R2Vect_dir1,localPolynomial_dir1$r.squared)

            localPolynomial_dir1 = rsq(glm(localCM_dir1 ~ poly(localMB_dir1, 3, raw = T)))
            # R2Vect_dir1 = c(R2Vect_dir1, localPolynomial_dir1)

            if(!is.na(localPolynomial_dir1) & !is.infinite(localPolynomial_dir1)){
                R2Vect_dir1 = c(R2Vect_dir1, localPolynomial_dir1)
            }else{
                localPolynomial_dir1 = 0.99
                R2Vect_dir1 = c(R2Vect_dir1, localPolynomial_dir1)
            }
        }else{
            localPolynomial_dir1 = 0.99
            R2Vect_dir1 = c(R2Vect_dir1, localPolynomial_dir1)
        }
    }

    # R-sequared for the 2nd direction : backwards
    R2Vect_dir2 = c()
    lastElemIndex = nrow(chrData)
    for(j in lastElemIndex:1){
        localMB_dir2 = chrData$chrPos[j:lastElemIndex]
        localCM_dir2 = chrData$chrCm[j:lastElemIndex]

        # if(j >= lastElemIndex-5){
        #     print(c("j = ", j))
        #     print(data.frame(localMB_dir2, localCM_dir2))
        #     print(glm(localCM_dir2 ~ poly(localMB_dir2, 3, raw = T)))
        #     print(lm(localCM_dir2 ~ poly(localMB_dir2, 3, raw = T)))
        # }

        if ((chrData$chrCm[j]!= 0) & (length(localMB_dir2) > 4)){
                localPolynomial_dir2 = rsq(glm(localCM_dir2 ~ poly(localMB_dir2, 3, raw = T)))

            if(!is.na(localPolynomial_dir2) & !is.infinite(localPolynomial_dir2)){
                R2Vect_dir2 = c(localPolynomial_dir2, R2Vect_dir2)
            }else{
                localPolynomial_dir2 = 0.99
                R2Vect_dir2 = c(localPolynomial_dir2, R2Vect_dir2)
                # print("dir 2 --> 0.99 returned from 1st else -> if(!is.na(localPolynomial_dir2))")
            }
        }else{
            localPolynomial_dir2 = 0.99
            R2Vect_dir2 = c(localPolynomial_dir2, R2Vect_dir2)
            # print("dir 2 --> 0.99 returned from 2nd else ->  if ((chrData$chrCm[j]!= 0) & (length(localMB_dir2) > 3))")
        }
        if(j >= lastElemIndex-5){
            print(data.frame(R2Vect_dir2))
        }
    }

    # print(R2Vect_dir1)
    # print(R2Vect_dir2)
    R2DataFrame2D = data.frame(R2Vect_dir1, R2Vect_dir2)

    return(R2DataFrame2D)
}
