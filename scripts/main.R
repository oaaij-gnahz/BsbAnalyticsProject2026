#%% read data
require("RFCDE")
source("R/data_loader.R")
data_dir_statsapi <- "C:/projections_data"
data_dir_baseballsavant <- "C:/Users/zhang/Rice-Courses/SMGT435_local/savant_data"
years <- list(2026)
data_event <- download_statsapi_eventdata_by_year(years, data_dir_statsapi)
data_savant <- download_baseballsavant_data_by_year(years, data_dir_baseballsavant)
#%%
# currently not considering foul balls, only batted balls in play or HR (ie. last pitch of at bat)
data_savant_lastpitches <- dplyr::bind_rows(data_savant) |>
  dplyr::group_by(game_id, year, event_index) |>
  dplyr::slice_max(order_by = pitch_number, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
#%%
data_agg <- data_event |>
  # remove bunts
  dplyr::filter(
    !grepl("Bunt", event)
  ) |>
  get_batter_events_statsapi() |>
  dplyr::left_join(data_savant_lastpitches, by = c("year", "game_id", "event_index"), suffix=c("", ".y")) |>
  dplyr::select(-ends_with(".y")) |>
  dplyr::filter(
    batter_event != "Not Batter Event",
    balls < 4,
    strikes < 3
  ) |>
  sabRmetrics::get_quadratic_coef(source="baseballsavant") |>
  sabRmetrics::get_trackman_metrics()
# calculate spray angle
data_agg_battedball <- data_agg |>
  dplyr::mutate(spray_angle = atan((hit_coord_x-125.43)/(198.27-hit_coord_y))/pi*180)
hist(data_agg_battedball$spray_angle)

# # look at pitches out of [-45, 45] spray angle range
# tmp_debug_data <- data_agg_battedball |>
#   dplyr::filter(spray_angle < -45)

#%% use RFCDE
# Parameters
n_trees <- 1000     # Number of trees in the forest
mtry <- 4           # Number of variables to potentially split at in each node
node_size <- 20     # Smallest node size
n_basis <- 15       # Number of basis functions
bandwidth <- 0.2    # Kernel bandwith - used for prediction only
lambda_param <- 10  # Poisson Process parameter

# Fit the model
input_feature_names <- c(
  "plate_x", "plate_z", "release_speed", "induced_vert_break",
  #"horz_break", "release_pos_x", "release_pos_z", "plate_time"
)
output_feature_names <- c("expected_woba")
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
# train it
print(sprintf("[INFO] Fitting RFCDE"))
start <- Sys.time()
functional_forest <- RFCDE::RFCDE(
  features_in, features_out,
  n_trees = n_trees, mtry = mtry, 
  node_size = node_size, n_basis = n_basis, 
  flambda = lambda_param,
  fit_oob = TRUE
  )
print(sprintf("RFCDE fitted in %.2f secs\n",
  as.numeric(difftime(Sys.time(), start, unit = "secs"))
))
