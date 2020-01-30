#' #' generate_stats
#' #'
#' #' xxxxx
#' #'
#' #' @param chromosome xx
#' #' @param RR_object xx
#' #' @param HCB_fileContent xx
#' #' @param do_cleaning xx
#' #'
#' #' @return statsObject
#' #' @export
#' #'
#'
#' generate_stats <- function(chromosome, RR_object, HCB_fileContent, do_cleaning){
#'
#'     chrSize = chromosome$mb[nrow(chromosome)]
#'     markers_nbr = nrow(chromosome)
#'     meanRR = mean(RR_object$regDr)
#'     minRR = min(RR_object$regDr)
#'     maxRR = max(RR_object$regDr)
#'
#'     # data cleaning step stats
#'     cleaningApplied = do_cleaning
#'     if(cleaningApplied){
#'         cleaningMode # % from size(chr) : primary (needed) or secondary (5%)
#'         nbr(removedMarkers)
#'         coords(removedMarkers)
#'     }
#'     size(centrome)
#'     size(telomere1)
#'     if(wholeChromosome){
#'         size(telomere2)
#'     }
#'
#'     #  chr type
#'     statsObject = data.frame()
#'     return(statsObject)
#' }
