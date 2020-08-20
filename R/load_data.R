#' @title load data
#' @description load data
#'
#' @rdname load_data
#' @export

load_data <- function(path_raw_data){
  raw_data  <-
    readxl::read_xlsx(here(path_raw_data),
                      sheet = "2017-2019PE数据")

  meta_data  <-  readxl::read_xlsx(here(path_raw_data),
                                   sheet = "变量说明")
  ## 更新 metadata

  # 删除一些原始数据表格的修改, 比如有些变量不见了, 变量的顺序改变了
  meta_data <-
    raw_data %>%
    colnames() %>%
    enframe() %>%
    dplyr::select(var = value) %>%
    left_join(meta_data) %>%
    bind_rows(meta_data %>% filter(var_mutate == "yes"))
  ## data type raw var
  cols_num <- meta_data %>%
    filter(var_mutate == "no") %>%
    filter(type == "num") %>%
    pull(var)

  cols_fac <- meta_data %>%
    filter(var_mutate == "no") %>%
    filter(type == "factor") %>%
    pull(var)

  raw_data <-
    raw_data %>%
    mutate_at(all_of(cols_num), as.numeric) %>%
    mutate_at(all_of(cols_fac), as.factor) %>%
    mutate(patient_id = as.character(patient_id))

  list("raw_data" = raw_data,
       "meta_data" = meta_data)
}
