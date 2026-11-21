source("R/data_loader.R")
source("R/utils.R")
# remove.packages("RFCDE")
devtools::load_all("~/R/my_external_libs/RFCDE/r")
require("RFCDE")
data_dir_statsapi <- "./data"
data_dir_baseballsavant <- "./data"
image_path <- "output/workspace_trackmanToExitVelo.RData"
years <- list(2026)
load(file="output/processed_data20261118.RData")
#%% use RFCDE
# Parameters
n_trees <- 100      # Number of trees in the forest
mtry <- 2           # Number of variables to potentially split at in each node
node_size <- 500     # Smallest node size
n_basis <- 5       # Number of basis functions
bandwidth <- "cv"    # Kernel bandwith - used for prediction only

# Fit the model
input_feature_names <- c(
  "plate_x", "plate_z", "release_speed", "induced_vert_break",
  "horz_break", "release_pos_x", "release_pos_z", "plate_time"
)
output_feature_names <- c("launch_speed")# c("launch_speed", "launch_angle", "spray_angle")
feature_names <- c(input_feature_names, output_feature_names)
model_data <- data_agg_battedball |>
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
cat("Input feature size:", dim(features_in), "\n")
cat("Output feature size:", dim(features_out), "\n")

# train it
print(sprintf("[INFO] Fitting RFCDE"))
start <- Sys.time()
rfcde_fit <- RFCDE::RFCDE(
  features_in, features_out,
  n_trees = n_trees, mtry = mtry, 
  node_size = node_size, n_basis = n_basis, 
  fit_oob = FALSE
  )
print(sprintf("RFCDE fitted in %.2f secs\n",
  as.numeric(difftime(Sys.time(), start, unit = "secs"))
))

#%% run prediction on the whole set
grid_out_cde <- make_feature_histogram_grid(features_out)# make grid
cde_grid_matrix <- do.call(rbind, grid_out_cde$centers) |> t()
print(sprintf("[INFO] Predicting with RFCDE"))
start <- Sys.time()
cde_pred <- predict(
  rfcde_fit, features_in,
  response = "CDE", z_grid = cde_grid_matrix, bandwidth = bandwidth
  )
print(sprintf("RFCDE predictions done in %.2f secs\n",
  as.numeric(difftime(Sys.time(), start, unit = "secs"))
))
saveRDS(cde_pred, file = sub("\\.RData$", "_cdePreds.rds", image_path))
saveRDS(cde_grid_matrix, file = sub("\\.RData$", "_cdeGrid.rds", image_path))
#%% join the predictions and group analysis
cbind_cde_data <- function(model_data, cde_matrix) {
  stopifnot(nrow(cde_matrix) == nrow(model_data))
  cde_cols <- paste0("cde_bin_", seq_len(ncol(cde_matrix)))
  cde_df <- as.data.frame(cde_pred)
  names(cde_df) <- cde_cols
  model_data_cde <- dplyr::bind_cols(model_data, cde_df)
  return(list(cde_cols=cde_cols, model_data_cde=model_data_cde))
}

bound_df <- cbind_cde_data(model_data, cde_pred)
cde_cols <- bound_df$cde_cols
model_data_cde <- bound_df$model_data_cde

group_avg <- model_data_cde |>
  dplyr::group_by(pitcher_id, pitch_type, zone) |>
  dplyr::summarise(
    n_batted_balls = dplyr::n(),
    dplyr::across(dplyr::all_of(cde_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
