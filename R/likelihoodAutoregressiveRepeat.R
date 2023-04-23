
#' Title
#'
#' @param listOfData
#' @param vectorOfParameters
#' @param yvar
#' @param ysize
#' @param Xvar
#' @param skipto
#'
#' @return
#' @export
#'
#' @examples
likelihoodAutoregressiveRepeat <- function(listOfData, vectorOfParameters, yvar, ysize,  Xvar, skipto){
  nLength <- length(listOfData)

  numberOfParameters <- length(vectorOfParameters)

  vectorOfLength <- unlist(lapply(listOfData, nrow))

  individualIntercept <- vectorOfParameters[1:nLength]
  vectorBeta <- vectorOfParameters[(nLength + 1):(nLength+length(Xvar))]
  alphaParameter <- vectorOfParameters[nLength+length(Xvar) + 1]
  deltaParameter <- vectorOfParameters[numberOfParameters]



  result <- 0
  for(site in 1:nLength){
    resultSite <- 0
    for(time in skipto:vectorOfLength[site]){
      fregression <- exp(individualIntercept[site]  + alphaParameter * listOfData[[site]][time-1, yvar]/listOfData[[site]][time-1, ysize] + sum(vectorBeta * listOfData[[site]][time,Xvar]))
      lambda <-   log(1+deltaParameter+fregression)
      resultSite <-  resultSite +   listOfData[[site]][time, yvar] / lambda +  listOfData[[site]][time, ysize] * log(lambda)
    }
    result <-  result + resultSite
  }

  result
}

