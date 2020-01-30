#' estimate_recombination_rates_third_degree_polynomial
#'
#' estimate recombination rates according to the MareyMap method using a 3rd degree polynomial that correlates between genetic and physical maps of a chromosome
#'
#' @param chromosome xx
#'
#' @import stats
#' @importFrom na.tools na.replace
#' @return RR_object
#' @export

estimate_recombination_rates_third_degree_polynomial <- function(chromosome){

    MB = chromosome$mb #physicalMap
    cM = chromosome$cm #geneticMap
    MB = as.numeric(MB)

    # regression function = 3rd degree polynomial
    model = glm(cM ~ poly(MB, 3, raw = T))
    print(model)
    regFn = predict(model, data.frame(MB))
    print(regFn)
    regDr = numericDeriv(quote(predict(model, data.frame(MB))), theta = "MB") # data.frame composed of regression function and its derivative values
    regDr = attr(regDr, "gradient") # aceess derivative values presented on the diagonal of attr object
    regDr = diag(regDr)
    regDr = as.numeric(regDr)

    # plot(MB, regDr, type = "l", col = "blue")
    # regFn[regFn < 0 ] = 0
    # regDr[regDr < 0 ] = 0

    # plot(MB, regDr, type="l", col="red")

    RR_object = data.frame(regFn, regDr) # data.frame composed of regression function and its derivative values

    return(RR_object)
}
