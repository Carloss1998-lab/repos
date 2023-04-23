
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
repeat_repos <- function(dataList, yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = repeat_opt){

      lenBlock <-  length(dataList)
      n_parameters <- lenBlock + length(Xvar) + 2

      likehood_dataList <- function(theta) likelihoodAutoregressiveRepeat(listOfData = dataList, vectorOfParameters = theta, yvar = yvar, Xvar=Xvar,
                                                                ysize= ysize, skipto = skipto)

      list_values <- numeric(repeat_opt)*NA
      mat_param <- matrix(NA, ncol = n_parameters, nrow = repeat_opt)
##############################optim ########################################################
      pb <- txtProgressBar(min = 0, max = repeat_opt, style = 3)
      for(needle_rep in 1:repeat_opt){
        theta_init <- c(rep(0, lenBlock), rep(0, length(Xvar)),  runif(2))
        res_opt <- tryCatch({
          optim(par = theta_init, fn = likehood_dataList,control = list(maxit = 1e6),lower = c(rep(-Inf, lenBlock+length(Xvar) + 1), 1e-6),
                                method = "L-BFGS-B")
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
################################################################################################

      label_var = colnames(dataList[[1]][,Xvar])
      parameters <- mat_param[which.min(list_values),]
      param_label = c(paste0("omega",1:lenBlock),label_var,"alpha","delta")

      loss_value <- min(list_values, na.rm = TRUE)
      matrices <- stdErrorAutoregressiveRepeat(dataList, parameters, yvar, ysize, Xvar ,  skipto)

      Information <-  matrices$Imatrix
      Sensibility <-  matrices$Jmatrix
      parameters_covariance <- solve(Sensibility)%*%Information%*%t(solve(Sensibility))/(nrow(dataList[[1]])-skipto)

      standard_deviation <- sqrt(diag(parameters_covariance))

      AIC <- 2*loss_value + 2 * n_parameters

      names(parameters) = names(standard_deviation) =param_label

      rownames(Information) = rownames(Sensibility) = rownames(parameters_covariance) = colnames(Information) = colnames(Sensibility) = colnames(parameters_covariance) = param_label

      list(parameters = parameters, standard_deviation = standard_deviation, AIC =  AIC,
           loss_value =  loss_value, Information = Information, Sensibility = Sensibility,
           parameters_covariance = parameters_covariance, nsample = nrow(nrow(dataList)-model_order),
           number_of_parameters = n_parameters , dataListMat = dataList)


    }



