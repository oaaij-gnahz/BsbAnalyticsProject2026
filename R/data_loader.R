require("sabRmetrics")

download_statsapi_eventdata_by_year <- function(years, cache_dir, bind_years = TRUE) {
  data_by_year <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[[i]]
    file_name <- sprintf("%s/data_%4d_event.csv", cache_dir, year)
    if (file.exists(file_name)) {
      print(sprintf("Loading %d", year))
      data_event_year <- read.csv(file_name, row.names = 1)
    } else {
      print(sprintf("Downloading %d", year))
      start_date <- sprintf("%4d-01-01", year)
      end_date <- sprintf("%4d-12-31", year)
      cluster <- parallel::makeCluster(parallel::detectCores())
      data_year <- sabRmetrics::download_statsapi(
        start_date = start_date,
        end_date = end_date,
        level = "MLB",
        game_type = "R",
        cl = cluster
      )
      write.csv(data_year$event, file = file_name)
      write.csv(data_year$pitch, file = sub("_event\\.csv$", "_pitch.csv", file_name))
      write.csv(data_year$play, file = sub("_event\\.csv$", "_play.csv", file_name))
      write.csv(data_year$game, file = sub("_event\\.csv$", "_game.csv", file_name))
      data_event_year <- data_year$event
    }
    data_by_year[[i]] <- data_event_year
  }
  if (bind_years) {
    return(dplyr::bind_rows(data_by_year))
  } else {
    return(data_by_year)
  }
}

download_baseballsavant_data_by_year <- function(years, cache_dir, bind_years = TRUE) {
  data_by_year <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[[i]]
    file_name <- sprintf("%s/data_%4d.csv", cache_dir, year)
    if (file.exists(file_name)) {
      print(sprintf("Loading %d", year))
      data_year <- read.csv(file_name, row.names = 1)
    } else {
      print(sprintf("Downloading %d", year))
      start_date <- sprintf("%4d-01-01", year)
      end_date <- sprintf("%4d-12-31", year)
      cluster <- parallel::makeCluster(parallel::detectCores())
      data_year <- sabRmetrics::download_baseballsavant(
        start_date = start_date,
        end_date = end_date,
        game_type = "R",
        cl = cluster
      )
      write.csv(data_year, file = file_name)
    }
    data_by_year[[i]] <- data_year
  }
  if (bind_years) {
    return(dplyr::bind_rows(data_by_year))
  } else {
    return(data_by_year)
  }
}

download_statsapi_pitchdata_by_year <- function(years, cache_dir, bind_years = TRUE) {
  data_by_year <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[[i]]
    file_name <- sprintf("%s/data_%4d_pitch.csv", cache_dir, year)
    if (file.exists(file_name)) {
      print(sprintf("Loading %d", year))
      data_pitch_year <- read.csv(file_name, row.names = 1)
    } else {
      print(sprintf("Downloading %d", year))
      start_date <- sprintf("%4d-01-01", year)
      end_date <- sprintf("%4d-12-31", year)
      cluster <- parallel::makeCluster(parallel::detectCores())
      data_year <- sabRmetrics::download_statsapi(
        start_date = start_date,
        end_date = end_date,
        level = "MLB",
        game_type = "R",
        cl = cluster
      )
      write.csv(data_year$pitch, file = file_name)
      write.csv(data_year$event, file = sub("_pitch\\.csv$", "_event.csv", file_name))
      write.csv(data_year$play, file = sub("_pitch\\.csv$", "_play.csv", file_name))
      write.csv(data_year$game, file = sub("_pitch\\.csv$", "_game.csv", file_name))
      data_pitch_year <- data_year$pitch
    }
    data_by_year[[i]] <- data_pitch_year
  }
  if (bind_years) {
    return(dplyr::bind_rows(data_by_year))
  } else {
    return(data_by_year)
  }
}

# preprocess events
event_table <- tibble::tribble(
  ~event,                         ~batter_event,
  "Balk",                         "Not Batter Event",
  "Batter Out",                   "Not Batter Event",
  "Bunt Groundout",               "Groundout",
  "Bunt Lineout",                 "Flyout",
  "Bunt Pop Out",                 "Flyout",
  "Catcher Interference",         "Not Batter Event",
  "Caught Stealing 2B",           "Not Batter Event",
  "Caught Stealing 3B",           "Not Batter Event",
  "Caught Stealing Home",         "Not Batter Event",
  "Double",                       "Double",
  "Double Play",                  "Flyout",
  "Field Error",                  "Reached on Error",
  "Field Out",                    "Groundout",
  "Fielders Choice",              "Groundout",
  "Fielders Choice Out",          "Groundout",
  "Flyout",                       "Flyout",
  "Forceout",                     "Groundout",
  "Game Advisory",                "Not Batter Event",
  "Grounded Into DP",             "Groundout",
  "Groundout",                    "Groundout",
  "Hit By Pitch",                 "Hit By Pitch",
  "Home Run",                     "Home Run",
  "Intent Walk",                  "Not Batter Event",
  "Lineout",                      "Flyout",
  "Passed Ball",                  "Not Batter Event",
  "Pickoff 1B",                   "Not Batter Event",
  "Pickoff 2B",                   "Not Batter Event",
  "Pickoff 3B",                   "Not Batter Event",
  "Pickoff Caught Stealing 2B",   "Not Batter Event",
  "Pickoff Caught Stealing 3B",   "Not Batter Event",
  "Pickoff Caught Stealing Home", "Not Batter Event",
  "Pop Out",                      "Flyout",
  "Runner Out",                   "Not Batter Event",
  "Sac Bunt",                     "Not Batter Event",
  "Sac Bunt Double Play",         "Not Batter Event",
  "Sac Fly",                      "Flyout",
  "Sac Fly Double Play",          "Flyout",
  "Single",                       "Single",
  "Stolen Base 2B",               "Not Batter Event",
  "Stolen Base 3B",               "Not Batter Event",
  "Stolen Base Home",             "Not Batter Event",
  "Strikeout",                    "Strikeout",
  "Strikeout Double Play",        "Strikeout",
  "Triple",                       "Triple",
  # TODO: Use batted ball trajectory for triple play 
  "Triple Play",                  "Flyout",
  "Walk",                         "Walk",
  "Wild Pitch",                   "Not Batter Event",
)

get_batter_events_statsapi <- function(data_table) {
  data_table |>
    dplyr::left_join(event_table, by = "event")
}

get_players <- function(years, cache_dir) {
  player_by_year <- lapply(years, function(year) {
    file_name <- sprintf("%s/player_%4d.csv", cache_dir, year)
    if (file.exists(file_name)) {
      print(sprintf("Loading player data %d", year))
      read.csv(file_name, row.names = 1)
    } else {
      print(sprintf("Downloading player data %d", year))
      player_year <- sabRmetrics::download_player(year)
      write.csv(player_year, file = file_name)
      player_year
    }
  })
  player <- dplyr::bind_rows(player_by_year) |>
    dplyr::distinct(player_id, .keep_all = TRUE)
}