
#' Title
#'
#' @param data
#' @param yvar
#' @param Xvar
#' @param ysize
#' @param model_order
#' @param skipto
#' @param repeat_opt
#'
#' @return
#' @export
#'
#' @examples
simple_repos <- function(data, yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = repeat_opt,stepwise = TRUE){

  likehood_data <- function(theta) likelihoodAutoregressive(data = data,  yvar = yvar, Xvar=Xvar,
                                                            ysize= ysize, model_order = model_order, skipto = skipto, vectorOfParameters = theta)

  if(!stepwise){
    list_values <- numeric(repeat_opt)*NA
    mat_param <- matrix(NA, ncol = model_order + length(Xvar) + 2, nrow = repeat_opt)
    pb <- txtProgressBar(min = 0, max = repeat_opt, style = 3)
    for(needle_rep in 1:repeat_opt){
      parauto <- rexp(model_order)
      theta_init <- c(rnorm(length(Xvar) + 1), parauto/(2*sum(parauto)), rexp(1))
      res_opt <- tryCatch({
        optim(theta_init, fn = likehood_data, control = list(maxit = 1e6), method = "L-BFGS-B")
      }, error = function(e) {
        return(NA)
      })

      if(length(res_opt)>1){
        list_values[needle_rep] <- res_opt$value
        mat_param[needle_rep, ] <- res_opt$par
      }

      setTxtProgressBar(pb, needle_rep)
    }
    close(pb)

    parameters <- mat_param[which.min(list_values),]
    loss_value <- min(list_values, na.rm = TRUE)
    matrices <- stdErrorAutoregressive(data, parameters, yvar, Xvar, ysize, model_order,  skipto)
    Information <-  matrices$Imatrix
    Sensibility <-  matrices$Jmatrix
    parameters_covariance <- solve(Sensibility)%*%Information%*%t(solve(Sensibility))/(nrow(data)-model_order)
    standard_deviation <- sqrt(diag(parameters_covariance))
    AIC <- 2*loss_value + 2 * (model_order + length(Xvar) + 2)

    list(parameters = parameters, standard_deviation = standard_deviation, AIC =  AIC,
         loss_value =  loss_value, Information = Information, Sensibility = Sensibility,
         parameters_covariance = parameters_covariance, nsample = nrow(nrow(data)-model_order),
         number_of_parameters = (model_order + length(Xvar) + 2), dataMat = data)
      }
  else{
      list_values <- numeric(repeat_opt)*NA
      mat_param <- matrix(NA, ncol = model_order + length(Xvar) + 2, nrow = repeat_opt)
      for(needle_rep in 1:repeat_opt){
        parauto <- rexp(model_order)
        theta_init <- c(rnorm(length(Xvar) + 1), parauto/(2*sum(parauto)), rexp(1))
        res_opt <- tryCatch({
          optim(theta_init, fn = likehood_data, control = list(maxit = 1e6), method = "L-BFGS-B")
        }, error = function(e) {
          return(NA)
        })

        if(length(res_opt)>1){
          list_values[needle_rep] <- res_opt$value
          mat_param[needle_rep, ] <- res_opt$par
        }
      }

      parameters <- mat_param[which.min(list_values),]
      loss_value <- min(list_values, na.rm = TRUE)
      matrices <- stdErrorAutoregressive(data, parameters, yvar, Xvar, ysize, model_order,  skipto)
      Information <-  matrices$Imatrix
      Sensibility <-  matrices$Jmatrix
      parameters_covariance <- solve(Sensibility)%*%Information%*%t(solve(Sensibility))/(nrow(data)-model_order)
      standard_deviation <- sqrt(diag(parameters_covariance))
      AIC <- 2*loss_value + 2 * (model_order + length(Xvar) + 2)

      list(parameters = parameters, standard_deviation = standard_deviation, AIC =  AIC,
           loss_value =  loss_value, Information = Information, Sensibility = Sensibility,
           parameters_covariance = parameters_covariance, nsample = nrow(nrow(data)-model_order),
           number_of_parameters = (model_order + length(Xvar) + 2), dataMat = data)

  }
}