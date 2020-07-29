#' estimate_recombination_rates_loess
#'
#' estimate recombination rates according to the MareyMap method using a 3rd degree polynomial that correlates between genetic and physical maps of a chromosome
#'
#' @param chromosome xx
#' @param spanVal xx
#'
#' @import stats
#' @importFrom na.tools na.replace
#' @return RR_object
#' @export

estimate_recombination_rates_loess <- function(chromosome, spanVal){

    MB = chromosome$mb #physicalMap
    cM = chromosome$cm #geneticMap
    MB = as.numeric(MB)

    # regression function = Loess using 2nd degree polynomial
    model = loess(cM ~ MB, span = spanVal, degree = 2)#, na.action = na.exclude
    regFn = predict(model, MB)  ## needed only for plotting
    MBplus1 = MB+1
    MBminus1 = MB-1
    cMplus1 = predict(model, MBplus1)
    cMplus1 = na.replace(cMplus1, max(cMplus1, na.rm = TRUE))
    cMminus1 = predict(model, MBminus1)
    cMminus1 = na.replace(cMminus1, min(cMminus1, na.rm = TRUE))
    regDr <- mapply(function(x1, x2, y1, y2) {round((y2-y1)/(x2-x1 ), 2)}, MBminus1, MBplus1, cMminus1, cMplus1)

    regDr[regDr < 0] = 0 # set negative values to zero

    # print(model)
    # print(data.frame(MBminus1, MBplus1, cMminus1, cMplus1, regFn2))
    # print(regDr)
    # plot(MB, regDr, type = "l", col = "red")

    RR_object = data.frame(regFn, regDr) # data.frame composed of regression function and its derivative values

    return(RR_object)
}
