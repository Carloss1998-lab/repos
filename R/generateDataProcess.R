#' Title
#'
#' @param Kvalue
#' @param Tvalue
#' @param paramDelta
#' @param paramOmega
#' @param paramAlpha
#' @param paramBeta
#' @param vectorSizeIntensity
#' @param init
#' @param xRates
#'
#' @return
#' @export
#'
#' @examples
gdp <- function(Kvalue, Tvalue, paramDelta, paramOmega, paramAlpha, paramBeta, vectorSizeIntensity, init, xRates){
  result <- vector(mode = 'list', length = Kvalue)
  for (kval in 1:Kvalue) {
    resultk <- matrix(data = NA, ncol = 2+length(xRates), nrow = 2*Tvalue)
    resultk[1,1] <- init
    resultk[1, 2] <- 1+rpois(n = 1, lambda = vectorSizeIntensity[[kval]])
    for (tval in 2:(2*Tvalue)) {
      ## Covariates update
      resultk[tval, 3:(2+length(xRates))] <- rexp(n = length(xRates), rate = xRates)
      ## lambda computation
      fregression <- exp(paramOmega + paramAlpha * resultk[tval - 1, 1]/resultk[tval - 1, 2]  +  sum(paramBeta * resultk[tval, 3:(2+length(xRates))]))
      lambda <- log(1 + paramDelta + fregression)
      ## update n value and y value
      resultk[tval, 2] <-  1+rpois(n = 1, lambda = vectorSizeIntensity[kval])
      resultk[tval, 1] <- sum(rexp(n = resultk[tval, 2], rate = 1/lambda))
    }
    result[[kval]] <- resultk[(Tvalue + 1):(2*Tvalue),]
    result[[kval]] <- as.data.frame(result[[kval]])
    names(result[[kval]]) <- c("yvar", "sizevar", paste0("X", 1:10))
  }
  result
}
