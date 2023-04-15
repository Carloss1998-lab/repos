
#' Title
#'
#' @param data
#' @param vectorOfParameters
#' @param yvar
#' @param Xvar
#' @param ysize
#' @param model_order
#' @param skipto
#'
#' @return
#' @export
#'
#' @examples
likelihoodAutoregressive <- function(data, vectorOfParameters, yvar, Xvar, ysize, model_order, skipto){


  tLength <-  nrow(data)
  numberOfCovariables <- length(Xvar)

  parametersVector <- numeric(length = 2 + model_order + numberOfCovariables)
  numberOfParameters <- length(parametersVector)

  intercept <- vectorOfParameters[1]
  vectorBeta <- vectorOfParameters[2:(length(Xvar)+1)]
  alphaParameter <- vectorOfParameters[(length(Xvar)+2):(numberOfParameters-1)]
  deltaParameter <- vectorOfParameters[numberOfParameters]

  result <- 0

  for(time in skipto:tLength){
    fregression <- exp(intercept  +
                         sum(alphaParameter * data[(time-1):(time-model_order), yvar]/data[(time-1):(time-model_order), ysize]) +
                         sum(vectorBeta * data[time,Xvar])
    )

    lambda <-   log(1+deltaParameter+fregression)
    result <-  result +   data[time, yvar] / lambda +  data[time, ysize] * log(lambda)
  }
  result
}

