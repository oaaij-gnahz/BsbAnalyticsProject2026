#%% read data
source("R/data_loader.R")
source("R/utils.R")
devtools::load_all("~/R/my_external_libs/RFCDE/r")
require("RFCDE")
data_dir_statsapi <- "./data"
data_dir_baseballsavant <- "./data"
image_path <- "output/workspace_trackmanBatsideTo3.Rdata"
years <- list(2026)
data_event <- download_statsapi_eventdata_by_year(years, data_dir_statsapi)
data_savant <- download_baseballsavant_data_by_year(years, data_dir_baseballsavant)
#%%
# currently not considering foul balls, only batted balls in play or HR (ie. last pitch of at bat)
print("Filtering for last pitch of at bat...")
data_savant_lastpitches <- dplyr::bind_rows(data_savant) |>
  dplyr::group_by(game_id, year, event_index) |>
  dplyr::slice_max(order_by = pitch_number, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
#%%
print("Removing bunts, non-better events and calculating Trackman metrics ...")
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
print("Calculating spray angle ...")
data_agg_battedball <- data_agg |>
  dplyr::mutate(
    spray_angle = atan((hit_coord_x-125.43)/(198.27-hit_coord_y))/pi*180,
    bat_side_factor = as.factor(bat_side)
    )
# hist(data_agg_battedball$spray_angle)

# # look at pitches out of [-45, 45] spray angle range
# tmp_debug_data <- data_agg_battedball |>
#   dplyr::filter(spray_angle < -45)
save.image(file="output/processed_data20261118.RData")
