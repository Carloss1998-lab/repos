
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
stdErrorAutoregressiveRepeat <- function(listOfData, vectorOfParameters, yvar, ysize, Xvar, skipto){


  nLength <- length(listOfData)

  numberOfParameters <- length(vectorOfParameters)

  vectorOfLength <- unlist(lapply(listOfData, nrow))

  individualIntercept <- vectorOfParameters[1:nLength]
  vectorBeta <- vectorOfParameters[(nLength + 1):(nLength+length(Xvar))]
  alphaParameter <- vectorOfParameters[nLength+length(Xvar) + 1]
  deltaParameter <- vectorOfParameters[numberOfParameters]


  I <- 0
  J <- 0

  for(site in 1:nLength){
    ISite <- 0
    JSite <- 0

    for(time in skipto:vectorOfLength[site]){
      fregression <- exp(individualIntercept[site]  + alphaParameter * listOfData[[site]][time-1, yvar]/listOfData[[site]][time-1, ysize] + sum(vectorBeta * listOfData[[site]][time,Xvar]))
      lambda <-   log(1+deltaParameter+fregression)

      partiambda <- c(unlist(c((1:nLength == site), listOfData[[site]][time,Xvar],  listOfData[[site]][time-1, yvar]/listOfData[[site]][time-1, ysize])) * (fregression /(1+ deltaParameter + fregression)), 1/(1+ deltaParameter + fregression))
      smS <- (1/lambda) * partiambda  * (
        listOfData[[site]][time,ysize] -(listOfData[[site]][time,yvar] / lambda)
      )
      smH <- (listOfData[[site]][time,ysize]/(lambda^2)) * partiambda %*% t(partiambda)

      ISite <- ISite + smS %*% t(smS)
      JSite <- JSite + smH
    }

    I <- I + ISite/(vectorOfLength[site]-skipto + 1)
    J <- J + JSite/(vectorOfLength[site]-skipto + 1)
  }

  return(list(Imatrix = I, Jmatrix = J))
}

