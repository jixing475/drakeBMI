#' @title tidy data
#' @description tidy data
#'
#' @rdname tidy_data
#' @export
#'
tidy_data <- function(raw_data, meta_data){
  data_tidy <-
    raw_data %>%
    # 替代实验变量中的 0 数值
    mutate_at(meta_data %>%
                filter(type == "num") %>%
                filter(result_group == "肝肾功能") %>%
                pull(var),
              ~ na_x2na(., x = 0) %>% na_x2na(., x = "/")
    ) %>%
    # PE 表型 ==============================
  mutate(age_group = if_else(matern_age >= 35, "高龄", "正常"),
         liver_abnormal = if_else(ALT > 35 | AST > 84, "是", "否"),
         alb_group = if_else(ALB < 25, "是", "否"),
         cr_group = if_else(Cr > 106, "是", "否"),
         plt_group = if_else(PLT < 100, "是", "否"),
         M_outcome = if_else(liver_abnormal == "是" |
                               cr_group == "是" |
                               NICU_admission == "1" |
                               ICU_admission == "1",
                             "是", "否")
  ) %>%
    mutate(
      infant_outcome = fct_relevel(infant_outcome,
                                   "活产",
                                   "围产死亡"),
      SGA_LGA = fct_relevel(SGA_LGA,
                            "正常体重",
                            "小于胎龄儿",
                            "大于胎龄儿")
    ) %>%
    # 两组 ==============================
  # mutate(
  #   BMI_group = case_when(
  #     Pre_BMI <= 22.9 ~ "Normal",
  #     Pre_BMI > 22.9 ~ "Obese"
  #   ))  %>%
  # dplyr::mutate(
  #   BMI_group = as.factor(BMI_group),
  #   BMI_group = fct_collapse(BMI_group,
  #                            `Normal` = c("Normal"),
  #                            `Obese` = c("Obese")),
  #   BMI_group = fct_relevel(BMI_group,
  #                           "Normal",
  #                           "Obese")) %>%
  # 三组  ==============================
  mutate(
    BMI_group = case_when(
      Pre_BMI <= 22.9 ~ "Normal",
      22.9 < Pre_BMI & Pre_BMI <= 24.9 ~ "Over weight",
      Pre_BMI > 24.9 ~ "Obese"
    ))  %>%
    dplyr::mutate(
      BMI_group = as.factor(BMI_group),
      BMI_group = fct_collapse(BMI_group,
                               `Normal` = c("Normal"),
                               `Over weight` = c("Over weight"),
                               `Obese` = c("Obese")),
      BMI_group = fct_relevel(BMI_group,
                              "Normal",
                              "Over weight",
                              "Obese"))
  # 四组 ==============================
  # mutate(
  #   BMI_group = case_when(
  #     Pre_BMI <= 18.5 ~ "体重不足",
  #     18.5 < Pre_BMI & Pre_BMI <= 23.9 ~ "体重正常",
  #     Pre_BMI > 23.9 & Pre_BMI <= 27.9 ~ "超重",
  #     Pre_BMI > 28  ~ "肥胖"
  #   ))  %>%
  # dplyr::mutate(
  #   BMI_group = as.factor(BMI_group),
  #   BMI_group = fct_collapse(BMI_group,
  #                            `体重不足` = c("体重不足"),
  #                            `体重正常` = c("体重正常"),
  #                            `超重` = c("超重"),
  #                            `肥胖` = c("肥胖")),
  #   BMI_group = fct_relevel(BMI_group,
  #                           "体重不足",
  #                           "体重正常",
  #                           "超重",
  #                           "肥胖")) %>%
  #set label for tidy data -------------------------------------------------------------
  cols_num <- meta_data %>%
    filter(type == "num") %>%
    pull(var)

  cols_fac <- meta_data %>%
    filter(type == "factor") %>%
    pull(var)

  data_tidy <-
    data_tidy %>%
    mutate_at(all_of(cols_num), as.numeric) %>%
    mutate_at(all_of(cols_fac), as.factor) %>%
    mutate(patient_id = as.character(patient_id))
  # set label for tidy data
  label_update <-
    colnames(data_tidy) %>%
    as_tibble() %>%
    left_join(meta_data %>% dplyr::select(var, label), by = c("value"="var")) %>%
    pull(label)

  var_label(data_tidy) <- label_update
  df <- data_tidy
  # 没有特别说明的话, 返回的的是最后一个赋值程序
  # 即使特别说明, 比如返回 return(data_tidy), 返回的也是最后一个 data_tidy <-
  # 不是最后一行 var_label(data_tidy) <- label_update 中的data_tidy
}
