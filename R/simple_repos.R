
#' Title
#'
#' @param data
#' @param yvar
#' @param Xvar
#' @param ysize
#' @param model_order
#' @param skipto
#' @param repeat_opt
#' @param stepwise
#'
#' @return
#' @export
#'
#' @examples
simple_repos <- function(data, yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = repeat_opt){

  likehood_data <- function(theta) likelihoodAutoregressive(data = data,  yvar = yvar, Xvar=Xvar,
                                                            ysize= ysize, model_order = model_order, skipto = skipto, vectorOfParameters = theta)

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

    label_var = colnames(data[, Xvar])
    parameters <- mat_param[which.min(list_values),]
    param_label = c("omega",label_var,paste0("alpha",1:model_order),"delta")
    loss_value <- min(list_values, na.rm = TRUE)
    matrices <- stdErrorAutoregressive(data, parameters, yvar, Xvar, ysize, model_order,  skipto)
    Information <-  matrices$Imatrix
    Sensibility <-  matrices$Jmatrix
    parameters_covariance <- solve(Sensibility)%*%Information%*%t(solve(Sensibility))/(nrow(data)-skipto)
    standard_deviation <- sqrt(diag(parameters_covariance))
    AIC <- 2*loss_value + 2 * (model_order + length(Xvar) + 2)
    names(parameters) = names(standard_deviation) =param_label
    rownames(Information) = rownames(Sensibility)=rownames(parameters_covariance) = colnames(Information) = colnames(Sensibility) = colnames(parameters_covariance) = param_label

    sign  <- unlist(lapply(parameters/standard_deviation,significat))

    summary.results <- cbind(round(parameters,3), round(standard_deviation,3),round(parameters/standard_deviation,3),sign)

    colnames(summary.results) <- c("parameters","std","t-value", "sig.code")


    results <- list(summary.results=summary.results,parameters = parameters, standard_deviation = standard_deviation, AIC =  AIC,
                    loss_value =  loss_value, Information = Information, Sensibility = Sensibility,
                    parameters_covariance = parameters_covariance, nsample = nrow(nrow(data)-model_order),
                    number_of_parameters = (model_order + length(Xvar) + 2), dataMat = data)
    class(results) <- "Repos"
    results
}

significat <- function(value){
  if (abs(value)>qnorm(0.999 + 0.001/2)){
    return("***")
  }else if (abs(value)>qnorm(0.99 + 0.01/2)){
    return("**")
    }else if(abs(value)>qnorm(0.95 + 0.05/2)){
    return("*")
  }else if(abs(value)>qnorm(0.9 + 0.1/2)){
    return(".")
  }
  }
