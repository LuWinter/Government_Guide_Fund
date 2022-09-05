
## 计算指标得分函数
make_score <- function(value_vec) {
  ### 计算分段值
  vec <- purrr::map_dbl(.x = seq(0, 1, 0.2)[-1], 
                        .f = \(x) quantile(value_vec, x, na.rm = TRUE))
  ### 向量化计算得分
  score_vec <- purrr::map_dbl(.x = value_vec,
                              .f = \(x) quantile_5(x, vec))
  score_vec
}

## 合并滞后指标函数
lag_n_year <- function(data, n, key, value, by) {
  data_copy <- data[, c(key, by, value)]
  data_copy[, c(by)] <- data_copy[, c(by)] + n
  lag_suffix <- paste0("_lag", n)
  data <- dplyr::left_join(data, data_copy, 
                           by = c(key, by),
                           suffix = c("", lag_suffix))
  data
}

## 分段赋值函数
quantile_5 <- function(value, sign_vec) {
  if (is.na(value)) {
    return(NA)
  }
  
  if (sign_vec[1] >= value) {
    return(1)
  } else if (sign_vec[1] < value & sign_vec[2] >= value) {
    return(2) 
  } else if (sign_vec[2] < value & sign_vec[3] >= value) {
    return(3)
  } else if (sign_vec[3] < value & sign_vec[4] >= value) {
    return(4)
  } else {
    return(5)
  }
}
