#' @title my transform
#' @description my transform
#'
#' @rdname my_boxcox
#' @export

my_boxcox <- function(y) {
  # hist(y, breaks = 12)
  p_value <- shapiro.test(y) %>% .$p
  if(p_value <= 0.05){
    result = MASS::boxcox(y ~ 1)
    (mylambda = result$x[which.max(result$y)])
    print(mylambda)
    y_trans = (y ^ mylambda - 1) / mylambda
    # hist(y2)
    return(y_trans)
  } else{
    print("don't run transform")
    return(y)
  }
}
