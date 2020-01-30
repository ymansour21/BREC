#' transform_inputData_to_chromosomes
#'
#' transform inputData to chromosomes including seperated chromosomal arms
#'
#'
#' @param inputData xxx
#' @param chrList xxx
#'
#' @return newInputData
#' @export

transform_inputData_to_chromosomes <- function(inputData, chrList){

    newInputData = data.frame()
    for (i in seq(1, length(chrList)-1, by =2 )){ # refine stop condition for the loop as per number of chromosomic arms + lonly arms must be at the end of the list
        if(i == 1){
            lenghtOfFirstArm = 23.513712 # size(2L_R6)
        }else if(i == 3){
            lenghtOfFirstArm = 28.110227 # size(3L_R6)
        }
        firstArm = get_chromosome_from_Arms(inputData, chrList[i])
        secondArm = get_chromosome_from_Arms(inputData, chrList[i+1])
        chromosome = transform_2arms_to_chromosome(firstArm, secondArm, lenghtOfFirstArm) # merge both arms into one chr : remember coord normalisation
        newInputData = rbind.data.frame(newInputData, chromosome)
    }
    chromosome = get_chromosome_from_Arms(inputData, chrList[length(chrList)])
    newInputData = rbind.data.frame(newInputData, chromosome)



  # newInputData = data.frame()
  # for (i in seq(1, length(chrList)-1, by =2 )){ # refine stop condition for the loop as per number of chromosomic arms + lonly arms must be at the end of the list
  #   firstArm = get_chromosome_from_Arms(inputData, chrList[i])
  #   secondArm = get_chromosome_from_Arms(inputData, chrList[i+1])
  #   chromosome = transform_2arms_to_chromosome(firstArm, secondArm, lenghtOfFirstArm) # merge both arms into one chr : remember coord normalisation
  #   newInputData = rbind.data.frame(newInputData, chromosome)
  # }
  # chromosome = get_chromosome_from_Arms(inputData, chrList[length(chrList)])
  # newInputData = rbind.data.frame(newInputData, chromosome)


  return(newInputData)
}
