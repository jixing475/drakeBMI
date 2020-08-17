#' @title get lambda value
#' @description get lambda value
#'
#' @rdname my_boxcox_get_lambda
#' @export

my_boxcox_get_lambda <- function(y) {
  #hist(y, breaks = 12)
  p_value <- shapiro.test(y) %>% .$p
  if(p_value <= 0.05){
    result = MASS::boxcox(y ~ 1)
    (mylambda = result$x[which.max(result$y)])
    #hist(y2)
    return(mylambda)
  } else{
    return(NA)
  }
}
