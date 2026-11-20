require("xgboost")

xlw3_i_feature_names = c("launch_speed", "launch_angle", "spray_angle")
xlw3_o_feature_names = c("linear_weight")

lookup_linearweight <- function(df_battedballs, df_lw) {
  df_battedballs |>
    dplyr::left_join(df_lw, by = dplyr::join_by(batter_event == event))
}

get_matrix_x <- function(df_battedballs) {
  df_battedballs |>
    dplyr::select(dplyr::all_of(xlw3_i_feature_names)) |>
    as.matrix()
}

get_matrix_y <- function(df_battedballs_with_lw) {
  df_battedballs_with_lw |>
    dplyr::select(dplyr::all_of(xlw3_o_feature_names)) |>
    as.matrix()
}


fit_xlw3 <- function(df_battedballs_with_lw, xgb_hparams) {
  matrix_x <- df_battedballs_with_lw |>
    get_matrix_x()
  matrix_y <- df_battedballs_with_lw |>
    get_matrix_y()
  xgb_xlw3 <- xgboost::xgboost(
    x = matrix_x, y = matrix_y, objective = "reg:squarederror",
    max_depth = xgb_hparams$max_depth,
    learning_rate = xgb_hparams$learning_rate,
    min_child_weight = xgb_hparams$min_child_weight
  )
  return(xgb_xlw3)
}

predict_xlw3 <- function(df_battedballs, xgb_xlw3_model) {
  matrix_x <- get_matrix_x(df_battedballs)
  predict(xgb_xlw3_model, newdata = matrix_x)
}

fit_estimate_xlw3 <- function(df_battedballs_with_lw, xgb_hparams) {
  model <- fit_xlw3(df_battedballs_with_lw, xgb_hparams)
  vec_pred <- predict_xlw3(df_battedballs_with_lw, model)
  return (list(model=model, predictions=vec_pred))
}
