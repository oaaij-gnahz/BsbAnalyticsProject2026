###
## TODO this script:
# estimate xLW3 with XGBoost (call R/xlw3.R) [TODO cross validation and hparam tuning]
# estimate batted ball trajectory from pitch tracking data with XGBoost
# bucket by pitcher and pitch type, and inspect the distribution in batted ball preds and corresponding xLW3 preds
#%% load data -----------
source("R/data_loader.R")
source("R/utils.R")
source("R/xlw3.R")
REDO <- FALSE
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
xlw3_model_path <- sprintf("output/xlw3_model_xgb_%s.rds", proc_idstr)
data_with_xlw3_path <- sprintf("output/battedball_with_xlw3_%s.rds", proc_idstr)
if (!REDO && file.exists(xlw3_model_path)) {
  xlw3_model <- readRDS(xlw3_model_path)
} else {
  print("Estimating XLW3")
  ts <- Sys.time()
  xlw3_model <- fit_xlw3(data_agg_battedball_with_lw, xgb_hparams)
  print(sprintf("XGBoost for XLW3 fitted in %.2f secs\n",
    as.numeric(difftime(Sys.time(), ts, unit = "secs"))
  ))
  saveRDS(xlw3_model, file = xlw3_model_path)
}

if (!REDO && file.exists(data_with_xlw3_path)) {
  data_with_xlw3 <- readRDS(data_with_xlw3_path)
} else {
  xlw3_preds <- predict_xlw3(data_agg_battedball_with_lw, xlw3_model)
  data_with_xlw3 <- data_agg_battedball_with_lw |>
    dplyr::mutate(xlw3 = xlw3_preds)
  saveRDS(data_with_xlw3, file = data_with_xlw3_path)
}

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
pred_cols <- paste0(output_feature_names, "_pred")
model_data_with_preds_path <- sprintf(
  "output/battedball_model_data_with_pred_%s.rds", proc_idstr
)
if (!REDO && file.exists(model_data_with_preds_path)) {
  model_data_with_preds <- readRDS(model_data_with_preds_path)
} else {
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
}


# xgb_hparams_grid <- expand.grid(
#   max_depth        = c(3, 5, 7),
#   min_child_weight = c(1, 8, 15),
#   learning_rate    = c(0.01, 0.1, 0.5),
#   val_nll          = 0
# )
if (REDO || !file.exists(model_data_with_preds_path)) {
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
  pred_df <- as.data.frame(batted_ball_traj_predmat)
  names(pred_df) <- pred_cols
  model_data_with_preds <- model_data |>
    cbind(pred_df)

  saveRDS(model_data_with_preds, file = model_data_with_preds_path)
}
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
model_data_pitch_avg_path <- sprintf(
  "output/battedball_group_avg_%s.rds", proc_idstr
)
if (!REDO && file.exists(model_data_pitch_avg_path)) {
  model_data_pitch_avg <- readRDS(model_data_pitch_avg_path)
} else {
  model_data_pitch_avg <- model_data_with_residuals |>
    dplyr::group_by(pitcher_id, pitch_type) |>
    dplyr::summarise(
      n_batted_balls = dplyr::n(),
      dplyr::across(dplyr::all_of(pred_cols), summarize_functions),
      dplyr::across(dplyr::all_of(output_feature_names), summarize_functions),
      dplyr::across(dplyr::all_of(paste0(output_feature_names, "_resid")), summarize_functions),
      .groups = "drop"
    )
  saveRDS(model_data_pitch_avg, file = model_data_pitch_avg_path)
}

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
make_battedball_histograms <- function(
    data, feature_cols, group_cols, bins = 20L, breaks = NULL) {
  # features <- c("launch_speed", "launch_angle", "spray_angle")
  # pred_features <- paste0(features, "_pred")
  # group_cols <- c("pitcher_id", "pitch_type", "bat_side")
  features <- feature_cols

  required <- c(group_cols, features)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (!all(vapply(data[features], is.numeric, logical(1)))) {
    stop("Observed and predicted feature columns must be numeric.")
  }

  if (is.null(breaks)) {
    bins <- as.integer(bins)

    # Shared edges make observed and predicted histograms comparable.
    breaks <- lapply(seq_along(features), function(j) {
      values <- data[[features[j]]]
      values <- values[is.finite(values)]
      if (!length(values)) {
        stop("No finite values for ", features[j], " or its predictions.")
      }

      limits <- range(values)
      if (limits[1L] == limits[2L]) {
        padding <- max(abs(limits[1L]) * 0.01, 0.5)
        limits <- limits + c(-padding, padding)
      }
      seq(limits[1L], limits[2L], length.out = bins + 1L)
    })
  } else if (
    !is.list(breaks) ||
      length(breaks) != length(features) ||
      !all(vapply(breaks, function(edge) {
        is.numeric(edge) && length(edge) >= 2L &&
          all(is.finite(edge)) && all(diff(edge) > 0)
      }, logical(1)))
  ) {
    stop("breaks must be a list of strictly increasing numeric edge vectors, one per feature.")
  }
  names(breaks) <- features

  histogram3d <- function(df, columns) {
    values <- as.matrix(df[, columns, drop = FALSE])
    keep <- apply(is.finite(values), 1L, all)
    values <- values[keep, , drop = FALSE]

    dimensions <- vapply(breaks, length, integer(1)) - 1L
    if (!nrow(values)) {
      return(array(0L, dim = dimensions))
    }

    indices <- lapply(seq_along(features), function(j) {
      findInterval(
        values[, j], breaks[[j]],
        rightmost.closed = TRUE,
        all.inside = TRUE
      )
    })
    linear_index <- indices[[1L]] +
      (indices[[2L]] - 1L) * dimensions[1L] +
      (indices[[3L]] - 1L) * dimensions[1L] * dimensions[2L]

    array(tabulate(linear_index, nbins = prod(dimensions)), dim = dimensions)
  }

  grouped <- dplyr::group_by(
    data, dplyr::across(dplyr::all_of(group_cols)), .drop = TRUE
  )
  keys <- dplyr::group_keys(grouped)
  groups <- dplyr::group_split(grouped)

  rows <- lapply(seq_along(groups), function(i) {
    le_hist <- histogram3d(groups[[i]], features)

    dplyr::bind_cols(
      keys[i, , drop = FALSE],
      tibble::tibble(
        group_size = sum(le_hist),
        feature_hist = list(le_hist),
        breaks = list(breaks)
      )
    )
  })

  dplyr::bind_rows(rows)
}


plot_battedball_histograms <- function(
    histograms, pitcher_id, pitch_type, bat_side,
    title_str,
    value_scale = c("count", "density")) {
  value_scale <- match.arg(value_scale)

  matches <- dplyr::filter(
    histograms,
    .data$pitcher_id == .env$pitcher_id,
    .data$pitch_type == .env$pitch_type,
    .data$bat_side == .env$bat_side
  )
  if (nrow(matches) != 1L) {
    stop("Expected exactly one histogram row for the requested group; found ",
         nrow(matches), ".")
  }

  row <- matches[1L, ]
  counts <- row$feature_hist[[1L]]
  breaks <- row$breaks[[1L]]
  feature_names <- names(breaks)
  pairs <- list(c(1L, 2L), c(1L, 3L), c(2L, 3L))

  plot_data <- lapply(pairs, function(pair) {
    marginal_counts <- apply(counts, pair, sum)
    x_breaks <- breaks[[pair[1L]]]
    y_breaks <- breaks[[pair[2L]]]
    x_centers <- (x_breaks[-1L] + x_breaks[-length(x_breaks)]) / 2
    y_centers <- (y_breaks[-1L] + y_breaks[-length(y_breaks)]) / 2

    values <- marginal_counts
    if (value_scale == "density") {
      total <- sum(marginal_counts)
      if (total <= 0) {
        stop("Cannot compute density for a group with no finite observations.")
      }
      cell_areas <- outer(diff(x_breaks), diff(y_breaks))
      values <- marginal_counts / (total * cell_areas)
    }

    result <- expand.grid(x = x_centers, y = y_centers)
    result$value <- as.vector(values)
    result$projection <- paste(
      feature_names[pair], collapse = "=X\nY="
    )
    result
  })

  plot_df <- dplyr::bind_rows(plot_data)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~projection, scales = "free") +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "steelblue",
      name = if (value_scale == "count") "Count" else "Density"
    ) +
    ggplot2::labs(
      title = paste(title_str),
    ) +
    ggplot2::theme_minimal()
}

feature_cols <- c("launch_speed", "launch_angle", "spray_angle")
pred_feature_cols <- paste0(feature_cols, "_pred")
group_cols <- c("pitcher_id", "pitch_type", "bat_side")

observed_histograms_path <- sprintf(
  "output/battedball_group_histograms_observed_%s.rds", proc_idstr
)
if (!REDO && file.exists(observed_histograms_path)) {
  battedball_histograms <- readRDS(observed_histograms_path)
} else {
  battedball_histograms <- make_battedball_histograms(
    model_data_with_residuals,
    feature_cols, group_cols,
    bins = 20L
  )
  saveRDS(battedball_histograms, file = observed_histograms_path)
}

battedball_pred_histograms <- make_battedball_histograms(
  model_data_with_residuals,
  pred_feature_cols, group_cols,
  breaks = battedball_histograms$breaks[[1]]
)


plot_and_save_onepitch <- function(battedball_histograms, pitcher_fullname, pitch_type_to_viz, save_path, title_str = NULL){
  font_size = 20
  if (is.null(title_str)) {
    title_str <- "Observed distribution"
  }
  pitcher_id <- player_name |>
    dplyr::filter(name_full == pitcher_fullname) |>
    dplyr::pull(player_id)
  hist_plot <- plot_battedball_histograms(
    battedball_histograms, pitcher_id = pitcher_id,
    pitch_type = pitch_type_to_viz, bat_side = "R",
    title_str = title_str, value_scale = "count"
  )
  hist_plot <- hist_plot +
    ggplot2::theme(
      text = ggplot2::element_text(size = font_size),
      plot.title = ggplot2::element_text(
        size = font_size,
      ),
      axis.title = ggplot2::element_text(size = font_size),
      axis.text = ggplot2::element_text(size = font_size * 0.8),
      strip.text = ggplot2::element_text(size = font_size * 0.9),
      legend.title = ggplot2::element_text(size = font_size),
      legend.text = ggplot2::element_text(size = font_size * 0.8)
    )
  ggplot2::ggsave(
    filename = save_path,
    plot = hist_plot,
    device = ragg::agg_png,
    width = 12,
    height = 4,
    units = "in",
    dpi = 300
  )
}

plot_and_save_onepitch(battedball_histograms, "Javier Assad", "FF", "output/observed_assad_ff_R.png", title_str = "Javier Assad Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Javier Assad", "SI", "output/observed_assad_si_R.png", title_str = "Javier Assad Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Javier Assad", "FC", "output/observed_assad_fc_R.png", title_str = "Javier Assad Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Bailey Ober", "FF", "output/observed_ober_ff_R.png", title_str = "Bailey Ober Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Bailey Ober", "CH", "output/observed_ober_ch_R.png", title_str = "Bailey Ober Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Bailey Ober", "SL", "output/observed_ober_sl_R.png", title_str = "Bailey Ober Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Jacob Misiorowski", "FF", "output/observed_misiorowski_ff_R.png", title_str = "Jacob Misiorowski Observed distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_histograms, "Jacob Misiorowski", "FC", "output/observed_misiorowski_fc_R.png", title_str = "Jacob Misiorowski Observed distribution based on Trackman metrics")


plot_and_save_onepitch(battedball_pred_histograms, "Javier Assad", "FF", "output/observed_assad_ff_R_pred.png", title_str = "Javier Assad Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Javier Assad", "SI", "output/observed_assad_si_R_pred.png", title_str = "Javier Assad Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Javier Assad", "FC", "output/observed_assad_fc_R_pred.png", title_str = "Javier Assad Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Bailey Ober", "FF", "output/observed_ober_ff_R_pred.png", title_str = "Bailey Ober Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Bailey Ober", "CH", "output/observed_ober_ch_R_pred.png", title_str = "Bailey Ober Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Bailey Ober", "SL", "output/observed_ober_sl_R_pred.png", title_str = "Bailey Ober Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Jacob Misiorowski", "FF", "output/observed_misiorowski_ff_R_pred.png", title_str = "Jacob Misiorowski Predicted distribution based on Trackman metrics")
plot_and_save_onepitch(battedball_pred_histograms, "Jacob Misiorowski", "FC", "output/observed_misiorowski_fc_R_pred.png", title_str = "Jacob Misiorowski Predicted distribution based on Trackman metrics")

battedball_histograms |>
  dplyr::left_join(player_name, by = dplyr::join_by(pitcher_id == player_id)) |>
  dplyr::arrange(-group_size)

plot_and_save_onepitch(battedball_histograms, "Cristopher Sánchez", "SI", "output/observed_sanchez_si_R.png", title_str = "Cristopher Sánchez - Observed batted ball distribution (SI, RHB)")
plot_and_save_onepitch(battedball_histograms, "Cristopher Sánchez", "CH", "output/observed_sanchez_ch_R.png", title_str = "Cristopher Sánchez - Observed batted ball distribution (CH, RHB)")
plot_and_save_onepitch(battedball_pred_histograms, "Cristopher Sánchez", "SI", "output/observed_sanchez_si_R_pred.png", title_str = "Cristopher Sánchez - Predicted batted ball distribution (SI, RHB)")
plot_and_save_onepitch(battedball_pred_histograms, "Cristopher Sánchez", "CH", "output/observed_sanchez_ch_R_pred.png", title_str = "Cristopher Sánchez - Predicted batted ball distribution (CH, RHB)")
