#' @title combine word file
#' @description combine word file

#' @rdname comebine_word
#' @export
#'

comebine_word <- function(final_res_group = "肝肾功能") {
  files_docx <-
    dir(here("analysis/data/derived_data/glm_res"), "\\.docx$",
        full.names = TRUE) %>%
    manuscriptsJX::vec_detect_keep(final_res_group)
  doc <- read_docx()
  for (i in 1:length(files_docx)) {
    print(i)
    doc <-
      doc %>%
      body_add_break(., pos = "after") %>%
      body_add_docx(src = files_docx[i])
  }
  print(doc, target = str_glue("~/Desktop/{final_res_group}.docx")
  )
}
