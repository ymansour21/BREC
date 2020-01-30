#' Save the generated interactive plot as png image
#'
#' Extract the interactive plot as static image saved in a png file
#'
#' @param RR_plot xx
#' @param plots_path xx
#' @param genomeName xx
#' @param chrID xx
#'
#' @return none
#' @export

save_plot_as_png <- function(RR_plot, plots_path, genomeName, chrID){

    # plotFileName = stringr::str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, ".png"), fixed(" "), "")

    # orca(RR_plot
    #      ,file = stringr::str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, ".png"), fixed(" "), "")
    #      ,format = tools::file_ext(file) # from tools package
    #      # ,width = 1400
    #      # ,height = 1200
    #      ,scale = NULL,
    #      mathjax = FALSE,
    #      parallel_limit = NULL,
    #      verbose = FALSE,
    #      debug = FALSE,
    #      safe = FALSE
    #      ) # to export png file from plotly

}
