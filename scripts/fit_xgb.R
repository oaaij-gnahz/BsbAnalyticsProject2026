###
## TODO this script:
# estimate xLW3 with XGBoost (call R/xlw3.R) [TODO cross validation and hparam tuning]
# estimate batted ball trajectory from pitch tracking data with XGBoost
# bucket by pitcher and pitch type, and inspect the distribution in batted ball preds and corresponding xLW3 preds
#%% load data -----------
source("R/data_loader.R")
source("R/utils.R")
source("R/xlw3.R")
# remove.packages("RFCDE")
# devtools::load_all("~/R/my_external_libs/RFCDE/r")
# require("RFCDE")
data_dir_statsapi <- "./data"
data_dir_baseballsavant <- "./data"
image_path <- "output/workspace_trackmanTo3_xgb.RData"
proc_idstr <- "xgb20261119"
years <- list(2026)
load(file="output/processed_data20261118.RData")

#%% fit xLW3
linear_weight <- read.csv("data/linear_weight.csv")
data_agg_battedball_with_lw <- lookup_linearweight(data_agg_battedball, linear_weight)
xgb_hparams <- list(
  max_depth = 5,
  min_child_weight = 15,
  learning_rate = 0.1
)
print("Estimating XLW3")
ts <- Sys.time()
xlw3_res <- fit_estimate_xlw3(data_agg_battedball_with_lw, xgb_hparams)
xlw3_preds = xlw3_res$xlw3_preds
xlw3_model = xlw3_res$model
print(sprintf("XGBoost for XLW3 fitted in %.2f secs\n",
  as.numeric(difftime(Sys.time(), ts, unit = "secs"))
))

data_with_xlw3 <- data_agg_battedball_with_lw |>
  dplyr::mutate(xlw3 = xlw3_preds)

saveRDS(xlw3_model, file = sprintf("output/xlw3_model_xgb_%s.rds", proc_idstr))
saveRDS(data_with_xlw3, file = sprintf("output/battedball_with_xlw3_%s.rds", proc_idstr))

cat("xLW3 RMSE:", sqrt(mean((data_with_xlw3$linear_weight - data_with_xlw3$xlw3)^2)), "\n")
cat("xLW3 mean signed error:", mean((data_with_xlw3$linear_weight - data_with_xlw3$xlw3)), "\n")
cat("xLW3 mean abs error:", mean(abs(data_with_xlw3$linear_weight - data_with_xlw3$xlw3)), "\n")

#%% prepare pitch tracking and output data for XGB
input_feature_names <- c(
  "plate_x", "plate_z", "release_speed", "induced_vert_break",
  "horz_break", "release_pos_x", "release_pos_z", "plate_time"
)
output_feature_names <- c("launch_speed", "launch_angle", "spray_angle")
feature_names <- c(input_feature_names, output_feature_names)
model_data <- data_with_xlw3 |>
  dplyr::filter(
    dplyr::if_all(dplyr::all_of(feature_names), ~ !is.na(.x))
  )

features_in <- model_data |>
  dplyr::select(dplyr::all_of(input_feature_names)) |>
  as.matrix()
features_out <- model_data |>
  dplyr::select(dplyr::all_of(output_feature_names)) |>
  as.matrix()
print(sprintf("[INFO] %d/%d qualifying batted ball events has non-NA feature values",
  nrow(model_data), nrow(data_agg_battedball)))
cat("Input feature size:", nrow(features_in), ncol(features_in), "\n")
cat("Output feature size:", nrow(features_out), ncol(features_out), "\n")


# xgb_hparams_grid <- expand.grid(
#   max_depth        = c(3, 5, 7),
#   min_child_weight = c(1, 8, 15),
#   learning_rate    = c(0.01, 0.1, 0.5),
#   val_nll          = 0
# )
#%% fit XGB model
fit_xgb <- function(data_x, data_y, xgb_hparams) {
  xgb_model <- xgboost::xgboost(
    x = data_x, y = data_y, objective = "reg:squarederror",
    max_depth = xgb_hparams$max_depth,
    learning_rate = xgb_hparams$learning_rate,
    min_child_weight = xgb_hparams$min_child_weight
  )
  return(xgb_model)
}
ts <- Sys.time()
xgb_model <- fit_xgb(features_in, features_out, xgb_hparams)
print(sprintf("XGBoost for pitch-battedball fitted in %.2f secs\n",
  as.numeric(difftime(Sys.time(), ts, unit = "secs"))
))
batted_ball_traj_predmat <- predict(xgb_model, newdata = features_in)
#%%add preds to our data frame
pred_cols <- paste0(output_feature_names, "_pred")
pred_df <- as.data.frame(batted_ball_traj_predmat)
names(pred_df) <- pred_cols
model_data_with_preds <- model_data |>
  cbind(pred_df)

saveRDS(model_data_with_preds, file = sprintf("output/battedball_model_data_with_pred_%s.rds", proc_idstr))
#%% inspect errors
calc_errors <- function(df, obs_col, pred_col) {
  rmse <- sqrt(mean((df[[obs_col]] - df[[pred_col]])^2))
  abse <- mean(abs(df[[obs_col]] - df[[pred_col]]))
  sgne <- mean(df[[obs_col]] - df[[pred_col]])
  rsqr <- 1 - sum((df[[obs_col]] - df[[pred_col]])^2) / sum((df[[obs_col]] - mean(df[[obs_col]]))^2)
  return(list(
    RMSE=rmse,
    mean_sgn_error=sgne,
    mean_abs_error=abse,
    r_squared=rsqr
  ))
}
df_errors <- list()
for (ofn in output_feature_names) {
  ofn_pred <- sprintf("%s_pred", ofn)
  errors <- calc_errors(model_data_with_preds, ofn, ofn_pred)
  errors$feature <- ofn
  df_errors[[length(df_errors)+1]] <- errors
}
df_errors <- do.call(rbind, lapply(df_errors, as.data.frame))
df_errors |>
  gt::gt() |>
  gt::fmt_number(
    columns = c("RMSE", "mean_sgn_error", "mean_abs_error"),
    decimals = 0) |>
  gt::fmt_number(columns = c("r_squared"), decimals = 2) |>
  gt::as_latex() |>
  sprintf() |>
  cat()
# TODO calculate xxlw3 from the predicted batted ball trajectory

#%% group by pitcher, pitch type and calculate mean and dispersion of batted ball trajectories
summarize_functions <-list(
  mean = ~ mean(.x, na.rm = TRUE),
  sd = ~ sd(.x, na.rm = TRUE),
  median = ~ median(.x, na.rm = TRUE),
  p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
  p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
  iqr = ~ quantile(.x, 0.75, na.rm = TRUE) - quantile(.x, 0.25, na.rm = TRUE)
)
model_data_with_residuals <- model_data_with_preds
for (feature in output_feature_names) {
  pred_col <- paste0(feature, "_pred")
  resid_col <- paste0(feature, "_resid")
  model_data_with_residuals[[resid_col]] <-
    model_data_with_residuals[[feature]] - model_data_with_residuals[[pred_col]]
}
model_data_pitch_avg <- model_data_with_residuals |>
  dplyr::group_by(pitcher_id, pitch_type) |>
  dplyr::summarise(
    n_batted_balls = dplyr::n(),
    dplyr::across(dplyr::all_of(pred_cols), summarize_functions),
    dplyr::across(dplyr::all_of(output_feature_names), summarize_functions),
    dplyr::across(dplyr::all_of(paste0(output_feature_names, "_resid")), summarize_functions),
    .groups = "drop"
  )
model_data_pitch_avg |>
  saveRDS(file = sprintf("output/battedball_group_avg_%s.rds", proc_idstr))

player <- get_players(years, data_dir_statsapi)
player_name <- player |>
  dplyr::select(player_id, name_full)

# model_data_pitch_avg |>
#   dplyr::left_join(player_name, by = dplyr::join_by(pitcher_id == player_id)) |>
#   dplyr::filter(n_batted_balls >= 20) |>
#   dplyr::arrange(spray_angle_pred_iqr) |>
#   dplyr::select(name_full, pitch_type, n_batted_balls, dplyr::matches("^spray_angle(_pred)?_(p25|p75|iqr)$")) |>
#   head(20) |>
#   View()

# model_data_pitch_avg |>
#   dplyr::left_join(player_name, by = dplyr::join_by(pitcher_id == player_id)) |>
#   dplyr::filter(n_batted_balls >= 20) |>
#   dplyr::arrange(launch_speed_resid_mean) |>
#   dplyr::select(name_full, pitch_type, n_batted_balls, launch_speed_pred_mean, launch_speed_mean, dplyr::matches("^launch_speed_resid_(mean|p25|p75)$")) |>
#   head(20) |>
#   View()

#%% observation only - launch_speed_angle analysis
source_data <- data_with_xlw3

lsa_categories <- if (is.factor(source_data$launch_speed_angle)) {
  levels(source_data$launch_speed_angle)
} else {
  sort(unique(source_data$launch_speed_angle[!is.na(source_data$launch_speed_angle)]))
}

launch_speed_angle_probs <- source_data |>
  dplyr::filter(!is.na(launch_speed_angle)) |>
  dplyr::count(
    pitcher_id, pitch_type, bat_side, launch_speed_angle,
    name = "n"
  ) |>
  dplyr::group_by(pitcher_id, pitch_type, bat_side) |>
  tidyr::complete(
    launch_speed_angle = lsa_categories,
    fill = list(n = 0L)
  ) |>
  dplyr::mutate(
    probability = n / sum(n),
    batted_balls = sum(n)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(
    names_from = launch_speed_angle,
    values_from = probability,
    names_prefix = "lsa_",
    values_fill = 0
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    entropy = {
      p <- dplyr::c_across(dplyr::starts_with("lsa_"))
      p <- p[!is.na(p) & p > 0]
      -sum(p * log(p))
    }
  ) |>
  dplyr::ungroup()

# launch_speed_angle_probs |>
#   dplyr::left_join(player_name, by = dplyr::join_by(pitcher_id == player_id)) |>
#   dplyr::filter(batted_balls >= 20) |>
#   dplyr::arrange(entropy) |>
#   dplyr::select(-pitcher_id) |>
#   head(20) |>
#   View()

#%% trivariate batted ball distributions
