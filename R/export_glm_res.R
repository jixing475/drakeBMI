#' @title export glm result
#' @description export glm result
#'
#' @rdname export_glm_res
#' @export

export_glm_res <- function(ft, para, dir) {
  doc <- read_docx() #create word with table
  body_add_par(doc, para, style = "heading 2")
  #body_add_fpar(doc, fpar(ftext(para, prop = my.prop)))# add para
  body_add_par(doc, " ") # add empty line
  # add table
  flextable::body_add_flextable(doc,
                                value = ft)
  fs::dir_create(here::here(
    str_glue("analysis/data/derived_data/{dir}")))

  print(doc, target = here::here(
    str_glue("analysis/data/derived_data/{dir}/{para}.docx")))
}
