#' @title create casewhen function
#' @description create casewhen function
#'
#' @examples
#'
#'
#' @rdname cw_apgar
#' @export
#'

cw_apgar <- casewhen::create_case_when(
  x >= 8 & x <= 10 ~ "0",
  x >= 4 & x <= 7 ~ "1",
  x >= 1 & x <= 3 ~ "2",
  x == 0 ~ "3",
  TRUE ~ as.character(x),
  vars = "x"
)
