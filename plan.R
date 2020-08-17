library(drake)
library(drakeBMI)


set_pkg()
path_raw_data <- "analysis/data/raw_data/raw_data.xlsx"

# plan ==============================
plan <- drake_plan(
  raw_data =
    readxl::read_xlsx(
      here(path_raw_data),
      sheet = "2017-2019PE数据"
    ),
  meta_data =
    readxl::read_xlsx(
      here(path_raw_data),
      sheet = "变量说明"
    ),
  #update metadata table 删除一些原始数据表格的修改, 比如有些变量不见了, 变量的顺序改变了
  meta_data =
    raw_data %>%
    colnames() %>%
    enframe() %>%
    dplyr::select(var = value) %>%
    left_join(meta_data) %>%
    bind_rows(meta_data %>% filter(var_mutate == "yes"))
)

# make ==============================
make(plan)
config <- drake_config(plan)
config$graph

vis_drake_graph(config, main = "",
                navigationButtons = TRUE, font_size = 15)
plan


