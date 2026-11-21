# Pitch-to-contact talent evaluation

To run existing analysis in a R terminal:
- First run `source("scripts/prepare_data.R")` - this downloads and preprocesses the batted ball data.
- Then run the following analysis scripts:
  - `source("scripts/fit_xgb.R")` - Fit xLW3 model and a model that predicts batted ball characteristics based on pitch data (Trackman features) and perform group analysis and visualization.
  - `source("scripts/fit_rfcde.R")` - Fit RFCDE model (currently still in process of debugging)