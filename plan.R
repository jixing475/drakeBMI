library(drakeBMI)
set_pkg()
availableCores()
future::plan(multiprocess, workers=8L)


path_raw_data <- "analysis/data/raw_data/raw_data.xlsx"
num_groups <- 3

# plan ==============================
plan <- drake_plan(
  datasets = load_data(path_raw_data),
  raw_data = datasets$raw_data,
  meta_data = datasets$meta_data,
  data_tidy = tidy_data(raw_data, meta_data)
)

# make ==============================
make(plan)
config <- drake_config(plan)
config$graph

vis_drake_graph(config, main = "",
                navigationButtons = TRUE, font_size = 15)
plan


