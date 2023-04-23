
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
repos <- function(data,  y, exp_var, size, model_order = 1, skipto = NULL, repeat_opt = 20){

  if(!is.list(data)){
    stop("Please, give a data.frame or list of data.frame.")
  }

  if(is.data.frame(data)){
    # check data type
    data <- as.matrix(data)
    Xvar <- which(colnames(data) %in% exp_var)
    yvar <- which(colnames(data) %in% y)
    ysize <- which(colnames(data) %in% size)


  if(is.null(skipto)) skipto = model_order + 1

      simple_repos(
        data = data,
        yvar = yvar,
        Xvar = Xvar,
        ysize = ysize,
        model_order = model_order,
        skipto = skipto,
        repeat_opt = repeat_opt
      )
  }else{

      data <- lapply(data,as.matrix)
      Xvar <- which(colnames(data[[1]]) %in% exp_var)
      yvar <- which(colnames(data[[1]]) %in% y)
      ysize <- which(colnames(data[[1]]) %in% size)
      repeat_repos(
        data = data,
        yvar = yvar,
        Xvar = Xvar,
        ysize = ysize,
        model_order = model_order,
        skipto = skipto,
        repeat_opt = repeat_opt
      )
  }
}
