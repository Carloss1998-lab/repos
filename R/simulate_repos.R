
#' Title
#'
#' @param Tvalue
#' @param paramDelta
#' @param paramOmega
#' @param paramAlpha
#' @param paramBeta
#' @param sizeIntensity
#' @param init
#' @param xRates
#' @param model_order
#'
#' @return
#' @export
#'
#' @examples
simulate_repos <- function(Tvalue, paramDelta, paramOmega, paramAlpha, paramBeta, sizeIntensity, init, xRates, model_order){
  result <- matrix(NA, nrow = model_order + 2* Tvalue, ncol = 2+length(xRates))

  result[1:model_order,1] <- init
  result[1:model_order, 2] <- 1+rpois(n = model_order, lambda = sizeIntensity)
  for (tval in (model_order+1):(model_order + 2*Tvalue)) {
    ## Covariates update
    result[tval, 3:(2+length(xRates))] <- rexp(n = length(xRates), rate = xRates)
    ## lambda computation
    fregression <- exp(paramOmega +
                         sum(paramAlpha * result[(tval - 1):(tval - model_order), 1]/result[(tval - 1):(tval - model_order), 2])  +
                         sum(paramBeta * result[tval, 3:(2+length(xRates))]))
    lambda <- log(1 + paramDelta + fregression)
    ## update n value and y value
    result[tval, 2] <-  1+rpois(n = 1, lambda = sizeIntensity)
    result[tval, 1] <- sum(rexp(n = result[tval, 2], rate = 1/lambda))
  }
  result <- result[(model_order + 1 + Tvalue):(model_order + 2*Tvalue),]
  names(result) <- c("yvar", "sizevar", paste0("X", 1:length(xRates)))

  result
}
