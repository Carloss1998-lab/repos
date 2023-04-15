
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
repos <- function(data,  y, exp_var, size, model_order = 2, skipto = 10, repeat_opt = 20, stepwise = FALSE){

  if (is.data.frame(data)) {
    data <- as.matrix(data)
    Xvar <- which(colnames(data) %in% exp_var)
    yvar <- which(colnames(data) %in% y)
    ysize <- which(colnames(data) %in% size)
  } else{
    stop("Please, give a data.frame.")
  }

  if(is.null(skipto)) skipto = model_order + 1


  if (!stepwise) {
    return(
      simple_repos(
        data = data,
        yvar = yvar,
        Xvar = Xvar,
        ysize = ysize,
        model_order = model_order,
        skipto = skipto,
        repeat_opt = repeat_opt,stepwise = stepwise
      )
    )
  } else{
    return(
      stepwise_repos(
        data = data,
        yvar = yvar,
        Xvar = Xvar,
        ysize = ysize,
        model_order = model_order,
        skipto = skipto,
        repeat_opt = repeat_opt,stepwise = stepwise
      )
    )

  }
}
