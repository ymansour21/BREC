#' transform_2arms_to_chromosome
#'
#' transform two related chromosomal arms to the corresponding chromosome
#'
#'
#' @param LeftArm xx
#' @param RightArm xx
#' @param lenghtOfFirstArm ..
#'
#' @return chromosome
#' @export

transform_2arms_to_chromosome <- function(LeftArm, RightArm, lenghtOfFirstArm){ # exp: 2L + 2R = 2

  RightArm$mb = RightArm$mb + lenghtOfFirstArm
  chromosome = rbind(LeftArm, RightArm)
  chromosome$chr = stringr::str_extract(chromosome$chr,'[0-9]+') # extract the chr number from it's string name
  return(chromosome)
}
