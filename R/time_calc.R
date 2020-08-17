#' @title time calc
#' @description time calc
#'
#'
#' @examples
#'
#'
#' @rdname time_calc
#' @export


time_calc <-
  function(string) {
    time_w_d <- string %>%
      str_split("\\+") %>% unlist()  %>%
      as.numeric()
    time = time_w_d[1] * 7 + time_w_d[2]
    return(time)
  }
