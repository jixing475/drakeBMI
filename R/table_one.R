#' @title create table one
#' @description create table one
#'
#'
#' @rdname table_one
#' @export

table_one <- function(data, path = "~/Desktop/example.docx"){
  restab <-
    data %>%
    compareGroups(BMI_group ~ . ,
                  data = .,
                  method = NA) %>%
    createTable(
      .,
      #digits = c(p14 = 0, hormo = 1),
      type = 2,
      show.all = TRUE,
      hide.no = "no",
      show.n = FALSE
    )
  export2word(restab,
              file = path,
              header.labels = c(p.overall = "p-value"))
  return(restab)
}
