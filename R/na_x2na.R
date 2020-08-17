#' @title turn x to NA
#' @description turn x to NA
#'
#'
#' @examples
#'
#' @importFrom purrr map_if
#' @importFrom magrittr %>%
#'
#' @rdname na_x2na
#' @export

na_x2na <- function (s, x = 0)
{
  sapply(s, function(y)
    ifelse(y %in% x, NA, y))
}

