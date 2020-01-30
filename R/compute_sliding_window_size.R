#' compute_sliding_window_size
#'
#' compute sliding window size to be used for chrmatin boundaries extraction
#'
#'
#' @param chromosome xx
#'
#' @return maxEcarts is the double value representing the size of the sliding window
#' @export

compute_sliding_window_size <- function(chromosome){ # needed for extract_CB

  ecartsVector = c()
  for(k in 1:(nrow(chromosome)-1)){
      # print("k compute swsz")
      # print(k)
      # print(chromosome$mb[k+1])
      # print( chromosome$mb[k])
# print(chromosome$mb[k+1] - chromosome$mb[k])
    ecartsVector = c(ecartsVector, chromosome$mb[k+1] - chromosome$mb[k])
  }
  # hist(ecartsVector)
  # print("from compute swsize : ecartsVector")
  # print(ecartsVector)
  maxEcarts = max(ecartsVector) # represents a chromosome-specefic sliding window size computed automatically
  return(maxEcarts)
}
