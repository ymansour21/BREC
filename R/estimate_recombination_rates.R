#' estimate_recombination_rates
#'
#' estimate recombination rates according to the MareyMap method using a 3rd degree polynomial that correlates between genetic and physical maps of a chromosome
#'
#' @param chromosome xx
#'
#' @import stats
#' @importFrom na.tools na.replace
#' @return RR_object
#' @export

estimate_recombination_rates <- function(chromosome){

  MB = chromosome$mb #physicalMap
  cM = chromosome$cm #geneticMap
  MB = as.numeric(MB)

  #---- Fitting regression models
  # 1st option : regression function = 3rd degree polynomial
  # model = glm(cM ~ poly(MB, 3, raw = T))
  # print(model)
  # regFn = predict(model, data.frame(MB))
  # print(regFn)
  # regDr = numericDeriv(quote(predict(model, data.frame(MB))), theta = "MB") # data.frame composed of regression function and its derivative values
  # regDr = attr(regDr, "gradient") # aceess derivative values presented on the diagonal of attr object
  # regDr = diag(regDr)
  # regDr = as.numeric(regDr)

  # plot(MB, regDr, type = "l", col = "blue")
  # regFn[regFn < 0 ] = 0
  # regDr[regDr < 0 ] = 0

  # 2nd option : regression function = Loess using 2nd degree polynomial
  model2 = loess(cM ~ MB, span = 0.25, degree = 2)#, na.action = na.exclude
  regFn2 = predict(model2, MB)  ## needed only for plotting
  MBplus1 = MB+1
  MBminus1 = MB-1
  cMplus1 = predict(model2, MBplus1)
  cMplus1 = na.replace(cMplus1, max(cMplus1, na.rm = TRUE))
  cMminus1 = predict(model2, MBminus1)
  cMminus1 = na.replace(cMminus1, min(cMminus1, na.rm = TRUE))
  regDr2 <- mapply(function(x1, x2, y1, y2) {round((y2-y1)/(x2-x1 ), 2)}, MBminus1, MBplus1, cMminus1, cMplus1)


  # print(model2)
  # print(data.frame(MBminus1, MBplus1, cMminus1, cMplus1, regFn2))
  # print(regDr2)
  # plot(MB, regDr2, type = "l", col = "red")

  # data.frame(regFn,regFn2, regDr, regDr2)

  # print("regDr2")
  # print(data.frame(regDr2, regDr))
  # plot(MB, regDr2, type="l", col="red")
  # lines(MB, regDr, type = "l", col = "blue")

  regFn = regFn2
  regDr = regDr2

  RR_object = data.frame(regFn, regDr) # data.frame composed of regression function and its derivative values

  return(RR_object)
}
