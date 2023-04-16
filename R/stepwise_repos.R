
stepwise_repos <- function(data, yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = NULL,stepwise = stepwise) {

  # Initialiser les variables
  print("Initialization")
  res <- simple_repos(data, y = yvar, Xvar = Xvar, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
  n <- nrow(data)
  vars_in <- seq_len(length(Xvar))
  vars_out <- integer(0)
  best_aic <- res$AIC
  # Initialisation de la barre de progression

  pb <- txtProgressBar(min = 0, max = length(vars_in), style = 3)

  # Boucle de sélection de variables par étape
  while (length(vars_in) > 0) {
    aics <- numeric(length(vars_in))
    for (i in c(1:length(vars_in))) {
      setTxtProgressBar(pb, i)
      x_in <- c(vars_out, vars_in[i])
      fit <- simple_repos(data, y = yvar, Xvar = x_in, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
      aics[i] <- fit$AIC
    }
    best <- which.min(aics)
    print(best)
    if (aics[best] < best_aic) {
      best_aic <- aics[best]
      vars_out <- c(vars_out, vars_in[best])
      vars_in <- vars_in[-best]
    } else {
      break
    }
  }

  # Fermer la barre de progression
  close(pb)
  x_selected <- Xvar[vars_out]
  final_var = colnames(data[, x_selected])
  result = simple_repos(data, y = yvar, Xvar = x_selected, ysize = ysize, model_order = model_order, skipto = skipto, repeat_opt = repeat_opt,stepwise = stepwise)
  result[["selected_variable"]] = final_var
  return(result)
}

# stepwise_repos <-function(data,  yvar, Xvar, ysize, model_order, skipto = NULL, repeat_opt = NULL){
#
# # Initialiser les variables
# res= simple_repos(data,y = yvar,
#            Xvar = Xvar,
#            ysize = ysize,
#            model_order = model_order, skipto = skipto,repeat_opt = repeat_opt)
# n <- nrow(repos_data)
# vars_in <- seq_len(length(Xvar))
# vars_out <- integer(0)
# best_aic <- res$AIC
#
# # Boucle de sélection de variables par étape
# while (length(vars_in) > 0) {
#   aics <- numeric(length(vars_in))
#   print("vars_out")
#   print(vars_out)
#   print("vars_in[i]")
#   print(vars_in[i])
#   print("aics")
#   print(aics)
#   for (i in vars_in) {
#     x_in <- c(vars_out, vars_in[i])
#     fit <- simple_repos(data,y = yvar,
#                         Xvar = x_in,
#                         ysize = ysize,
#                         model_order = model_order, skipto = skipto,repeat_opt = repeat_opt)
#     aics[i] <- fit$AIC
#   }
#   best <- which.min(aics)
#   if (aics[best] < best_aic) {
#     print("oui")
#     best_aic <- aics[best]
#     vars_out <- c(vars_out, vars_in[best])
#     vars_in <- vars_in[-best]
#   } else {
#     break
#   }
# }
# x_selected <- x[, vars_out]
# return(list(simple_repos(data,y = yvar,
#                     Xvar = x_selected,
#                     ysize = ysize,
#                     model_order = model_order ,skipto = skipto,repeat_opt = repeat_opt)
#        ),selected_variable = colnames(data[,Xvar])[x_selected])
#
# }


# Réaliser une régression avec les variables sélectionnées
# x_selected <- x[, vars_out]
# fit <- poly_autoregression(y, x_selected)
#
# # Afficher le modèle final sélectionné
# summary(fit)
