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
      show.p.mul = TRUE, # 显示过个 P value, 两两之间的比较
      extra.labels = c("Mean (SD)","Median (IQR)","",""), # 给变量添上额外标签
      digits.p = 3,
      sd.type = 2, # 是() 还是 ±
      show.all = TRUE,
      hide.no = "no",
      show.n = FALSE
    )
  export2word(restab,
              file = path,
              header.labels = c(p.overall = "p-value"))
  return(restab)
}
