
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
stepwise_repos <- function(data, yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = NULL,stepwise = stepwise) {

  # Initialiser les variables
  print("Initialization")
  res <- simple_repos(data, y = yvar, Xvar = Xvar, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
  n <- nrow(data)
  vars_in <- Xvar
  vars_out <- integer(0)
  best_aic <- res$AIC
  # Initialisation de la barre de progression
  print("Feature selection")

  pb <- txtProgressBar(min = 0, max = length(vars_in), style = 3)

  back = TRUE
  # Boucle de sélection de variables par étape
  while (length(vars_in) > 0) {
    if(back){
      aics <- numeric(length(vars_in))
    for (i in c(1:length(vars_in))) {
      print("forw")
      setTxtProgressBar(pb, i)
      x_in <- c(vars_out, vars_in[i])
      print(x_in)
      fit <- simple_repos(data, y = yvar, Xvar = x_in, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
      aics[i] <- fit$AIC
    }
      best <- which.min(aics)
      if(aics[best] > best_aic){back = FALSE}
    }

    if(!back){
      aics <- numeric(length(vars_in))
      x_in_list = list()
      for (i in c(1:length(vars_in))) {
        setTxtProgressBar(pb, i)
        x_in <- vars_in[-i]
        print("back")
        print(x_in)
        fit <- simple_repos(data, y = yvar, Xvar = x_in, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
        aics[i] <- fit$AIC
        x_in_list[[i]] = x_in
      }
      best <- which.min(aics)
    }
    print("aics")
    print(aics)
    print("best")
    print(best_aic)
    if (aics[best] < best_aic) {
      best_aic <- aics[best]
      vars_in <- x_in_list[[best]]
      print("vars_in")
      print(vars_in)
      vars_out <- vars_in
    } else {
      break
    }
  }

  # Fermer la barre de progression
  close(pb)
  print(vars_out)
  if(length(vars_out)>0){
    x_selected <- Xvar[vars_out]
    final_var = colnames(data)[x_selected]
    print("Final model computing...")
    result = simple_repos(data, y = yvar, Xvar = x_selected, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
    result[["selected_variable"]] = final_var
    return(result)
  }else{
    final_var = colnames(data)[Xvar]
    res[["selected_variable"]] = final_var
    return(res)
  }

}
