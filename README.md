# Machine Learning and Probabilistic Approach for Rt and COVID-19 Case Forecasting

## Contents

### 1. Data

- **County-Level COVID-19 Case Data:** COVID-19 daily case data at the county level in South Carolina (SC) was used for the analysis. The data was filtered from the US county-level data available at:
  - https://github.com/nytimes/covid-19-data/blob/master/us-counties-2020.csv
  - https://github.com/nytimes/covid-19-data/blob/master/us-counties-2021.csv
  - https://github.com/nytimes/covid-19-data/blob/master/us-counties-2022.csv
  - https://github.com/nytimes/covid-19-data/blob/master/us-counties-2023.csv

  The filtered SC county-level data is also provided in the **Covid_Case_Data_SC_Counties/** folder.

### The Data folder contains:

#### Folders
- **Covid_Case_Data_SC_Counties/** — Filtered SC county-level COVID-19 case data.
- **Forecast_Accuracy_Summary/** — Summary of forecast accuracy metrics across models.
- **Forecast_Scenario-1_07_day/** — Scenario 1 forecasts with a 7-day horizon.
- **Forecast_Scenario-1_14_day/** — Scenario 1 forecasts with a 14-day horizon.
- **Forecast_Scenario-1_21_day/** — Scenario 1 forecasts with a 21-day horizon.
- **Forecast_Scenario-2_07_day/** — Scenario 2 forecasts with a 7-day horizon.
- **Forecast_Scenario-2_14_day/** — Scenario 2 forecasts with a 14-day horizon.
- **Forecast_Scenario-2_21_day/** — Scenario 2 forecasts with a 21-day horizon.

#### Files
- **Rt_Estimates_Initial.csv** — Initial Rt estimates obtained using the EpiNow2 R package.
- **Rt_Estimates_Smooth.csv** — Spatially smoothed Rt estimates using the INLA model.
- **SC_county_sociodemographic_data.csv** — Sociodemographic data for SC counties used as covariates.
- 

## 2. Rt Estimation Using EpiNow2

### Setup

1. Set the working directory to the folder where your SC/US county-level COVID-19 data files are saved:
```r
   setwd("path/to/your/data/directory")
```

2. Update the **start and end dates** for the wave you want to analyze. These need to be changed in two places in the code:
   - Data filtering step (~line 55): `date >= "2022-05-01" & date <= "2023-02-11"`
   - Rt estimation loop (~line 155): `start_date` and `end_date` assignments

3. To enable **forecasting of Rt and cases**, set the forecast horizon in the `epinow()` call (~line 136):
```r
   forecast = forecast_opts(horizon = 14)  # set to desired number of days (7, 14, 21); 0 = no forecast
```

4. After running, save the results as `Rt_Estimates_Initial`:
```r
   Rt_Estimates_Initial <- Rt_county
   write.csv(Rt_Estimates_Initial, "Rt_Estimates_Initial.csv", row.names = FALSE)
```

## 3. Spatial Smoothing of Rt Using INLA

This step applies a Besag spatial model via INLA to smooth the initial Rt estimates across SC counties, using sociodemographic covariates.

### Inputs

- **Rt_Estimates_Initial.csv** — Output from the previous EpiNow2 step.
- **SC_county_sociodemographic_data.csv** — County-level sociodemographic data (provided in the Data folder).
- SC county shapefiles are downloaded automatically via the `tigris` package.

### Setup

1. Set the working directory to the folder containing `Rt_Estimates_Initial.csv` and `SC_county_sociodemographic_data.csv`:
```r
   setwd("path/to/your/data/directory")
```

2. Run the INLA spatial smoothing script. The code will:
   - Load the initial Rt estimates and sociodemographic data.
   - Download SC county shapefiles and build a spatial adjacency matrix.
   - Fit a Besag spatial model for each date using covariates (age groups, SVI, insurance, race, income, employment, etc.).
   
3. The output is saved as:
```r
   write.csv(Rt_county, "Rt_Estimates_Smooth.csv", row.names = FALSE)
```
## 4. Individual Model Forecasts

Forecast results for Rt and COVID-19 cases were generated using various models, including Random Forest (RF), Regression, and XGBoost. The forecasting procedure is similar across all models. Below are the instructions using the XGBoost model as an example.

### Inputs

- **Rt_Estimates_Initial.csv** or **Rt_Estimates_Smooth.csv** — Rt estimates from Step 2 or Step 3. The forecasting is performed separately for both datasets.
- **SC county-level COVID-19 case data** — CSV files from the `Covid_Case_Data_SC_Counties/` folder.

### Setup

1. Set the working directory and load the data:
```r
   setwd("path/to/your/data/directory")
   Rt_data <- read.csv("Rt_estimates_initial.csv")  # or "Rt_estimates_smooth.csv"
```

2. Update the **training period** dates:
```r
   train_start_date <- as.Date("2022-05-01")
   train_end_date <- as.Date("2023-12-10")
```

3. Update the **forecasting period** dates:
```r
   start_date <- "2022-12-11"
   end_date <- "2023-03-04"
```

4. Set the **forecast step** (horizon) to 7, 14, or 21 days:
```r
   forecast_step <- 7  # change to 14 or 21 as needed
```

### What the Code Does

- **Hyperparameter optimization:** Performs cross-validation grid search over lag values, learning rate (`eta`), L2 regularization (`lambda`), and tree depth (`max_depth`) to find the best XGBoost model.
- **Recursive Rt forecasting:** Trains the final model on data up to the forecast start date, then recursively forecasts Rt for each county over the forecast horizon.
- **COVID-19 case forecasting:** Utilize the Rt forecasts into COVID-19 case forecasts using a Poisson stochastic process with a discretized gamma serial interval distribution.
- **Rolling forecast windows:** The Rt and COVID-19 case forecasts are performed in rolling windows of `forecast_step` days from `overall_start_date` to `overall_end_date`.

### Output

After running, save the results:
```r
write.csv(all_forecast_results, "Forecast_XGB.csv", row.names = FALSE)
```

The output contains columns for County, Date, actual and forecasted Rt, actual and forecasted COVID-19 cases for Rt, and COVID-19 case forecasts.

> **Note:** The same procedure applies for Random Forest (RF) and Regression models — only the model fitting and prediction functions differ. Update the training/forecasting dates and forecast step accordingly for each model. Run each model separately for both `Rt_Estimates_Initial.csv` and `Rt_Estimates_Smooth.csv` to generate the two sets of forecasts.

## 5. Ensemble Forecasts

The ensemble method combines Rt forecasts from all six individual models (XGBoost, Random Forest, and Regression — each run on both `Rt_Estimates_Initial` and `Rt_Estimates_Smooth`) to produce a weighted ensemble forecast for Rt and COVID-19 cases.

### Inputs

- **Forecast_XGB.csv** — XGBoost forecasts using initial Rt estimates.
- **Forecast_XGB_Smooth.csv** — XGBoost forecasts using smoothed Rt estimates.
- **Forecast_RF.csv** — Random Forest forecasts using initial Rt estimates.
- **Forecast_RF_Smooth.csv** — Random Forest forecasts using smoothed Rt estimates.
- **Forecast_Reg.csv** — Regression forecasts using initial Rt estimates.
- **Forecast_Reg_Smooth.csv** — Regression forecasts using smoothed Rt estimates.
- **SC county-level COVID-19 case data** — CSV files from the `Covid_Case_Data_SC_Counties/` folder.

### Setup

1. Set the working directory and load all six forecast files:
```r
   setwd("path/to/your/data/directory")
```

2. Update the **training period** and **forecasting period** dates:
```r
   train_start_date <- as.Date("2020-06-01")
   train_end_date <- as.Date("2020-11-10")

   start_date <- "2020-11-11"
   end_date <- "2021-02-02"
```

3. Set the **forecast step** (horizon) to 7, 14, or 21 days:
```r
   forecast_step <- 7  # change to 14 or 21 as needed
```

4. Update the **overall forecasting window**:
```r
   overall_start_date <- as.Date("2020-11-11")
   overall_end_date <- as.Date("2021-02-02")
```

### What the Code Does

- **Weight calculation:** Computes Percentage Agreement (PA) between actual and forecasted Rt for each of the six models. The PA-based weights are calculated as each model's PA divided by the sum of all models' PA values.
- **Ensemble Rt forecast:** Produces a weighted combination of Rt forecasts from all six models.
- **COVID-19 case forecast:** Converts the ensemble Rt forecasts into COVID-19 case forecasts using a Poisson stochastic process with a discretized gamma serial interval distribution.
- **Rolling forecast windows:** The ensemble Rt and COVID-19 case forecasts are performed in rolling windows of `forecast_step` days from `overall_start_date` to `overall_end_date`.

### Output

After running, save the results:
```r
write.csv(all_forecast_results, "Forecast_Ensemble.csv", row.names = FALSE)
```

The output contains columns for County, Date, actual and ensemble-forecasted Rt, actual and forecasted COVID-19 cases.

## 6. Forecast Summary Statistics

The `Forecast_Accuracy_Summary/` folder contains scenario-based summary statistics for Rt and COVID-19 case forecasts. These include accuracy metrics (e.g., Percentage Agreement, RMSE, MAE) computed across all models and the ensemble method for each forecast scenario (7-day, 14-day, and 21-day horizons).

## 7. Figures

The Figures folder contains:

- **Main Figures (Fig. 1, Fig. 2, Fig. 3):** Visualizations used in the primary analysis and manuscript.
- **Supplementary Figures (Fig. S1 – Fig. S27):** Additional visualizations supporting the analysis.

Contact:
For questions, please contact:
Md Sakhawat Hossain
Email: mdsakhh@clemson.edu
