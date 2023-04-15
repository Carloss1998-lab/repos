
#' Title
#'
#' @param listOfData
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
stdErrorAutoregressive <- function(listOfData, vectorOfParameters, yvar, Xvar, ysize, model_order,  skipto){


  tLength <-  nrow(listOfData)
  numberOfCovariables <- length(Xvar)

  parametersVector <- numeric(length = 2 + model_order + numberOfCovariables)
  numberOfParameters <- length(parametersVector)

  intercept <- vectorOfParameters[1]
  vectorBeta <- vectorOfParameters[2:(length(Xvar)+1)]
  alphaParameter <- vectorOfParameters[(length(Xvar)+2):(numberOfParameters-1)]
  deltaParameter <- vectorOfParameters[numberOfParameters]



  I <- 0
  J <- 0



  for(time in skipto:tLength){
    fregression <- exp(intercept  + sum(alphaParameter * listOfData[(time-1):(time-model_order), yvar]/listOfData[(time-1):(time-model_order), ysize])
                       + sum(vectorBeta * listOfData[time,Xvar]))
    lambda <-   log(1+deltaParameter+fregression)

    partialLambda <- c(c(1, listOfData[time,Xvar], listOfData[(time-1):(time-model_order), yvar]/listOfData[(time-1):(time-model_order), ysize] ) * (fregression /(1+ deltaParameter + fregression)), 1/(1+ deltaParameter + fregression))
    smallS <- (1/lambda) * partialLambda  * (
      listOfData[time,ysize] -(listOfData[time,yvar] / lambda)
    )
    smallH <- (listOfData[time,ysize]/(lambda^2)) * partialLambda %*% t(partialLambda)

    I <- I + smallS %*% t(smallS)/(tLength-skipto + 1)
    J <- J + smallH/(tLength-skipto + 1)
  }


  return(list(Imatrix = I, Jmatrix = J))
}
