#' get_min_RR_value_based_on_polynomial
#'
#' ........
#'
#' @param chromosome xx
#'
#' @import stats
#' @importFrom na.tools na.replace
#' @return minRR_object
#' @export

get_min_RR_value_based_on_polynomial <- function(chromosome){

    MB = chromosome$mb #physicalMap
    cM = chromosome$cm #geneticMap
    MB = as.numeric(MB)

    #---- Fitting regression models
    #  regression function = 3rd degree polynomial
    model = glm(cM ~ poly(MB, 3, raw = T))
    # print(model)
    regFn = predict(model, data.frame(MB))
    # print(regFn)
    regDr = numericDeriv(quote(predict(model, data.frame(MB))), theta = "MB") # data.frame composed of regression function and its derivative values
    regDr = attr(regDr, "gradient") # aceess derivative values presented on the diagonal of attr object
    regDr = diag(regDr)

    # regDr[regDr < 0] = 0 # set negative values to zero
    # print("from get min RR")
    # print(regDr)

    minRRpoly = min(regDr)
    indexOfMinRRpoly = match(minRRpoly, regDr)
    minRR_object = data.frame(minRRpoly, indexOfMinRRpoly)

    return(minRR_object)

}
