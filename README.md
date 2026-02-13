
# Rt Estimation Using EpiNow2

## Setup

1. Set the working directory to the folder where your SC/US county-level COVID-19 data files are saved:
```r
   setwd("path/to/your/data/directory")
```

2. Update the **start and end dates** for the wave you want to analyze. These need to be changed in two places in the code:
   - Data filtering step (~line 55): `date >= "2022-05-01" & date <= "2023-02-11"`
   - Rt estimation loop (~line 155): `start_date` and `end_date` assignments

3. To enable **forecasting of Rt and cases**, set the forecast horizon in the `epinow()` call (~line 136):
```r
   forecast = forecast_opts(horizon = 14)  # set to desired number of days (7,14,21); 0 = no forecast
```

4. After running, **save the results** as `Rt_Estimates_Initial`:
```r
   Rt_Estimates_Initial <- Rt_county
   Save as csv file.
```



# Machine-Learning-and-Probabilistic-Approach-for-Rt-and-COVID-19-Case-Forecasting
Machine Learning and Probabilistic Approach for Rt and COVID-19 Case Forecasting
Contents
1. Data
•	County Level COVID-19 Case Data: COVID-19 daily case data at the county level in South Carolina (SC) was used for the analysis.All the case data files are available at:
https://github.com/nytimes/covid-19-data/blob/master/us-counties-2020.csv
https://github.com/nytimes/covid-19-data/blob/master/us-counties-2021.csv
https://github.com/nytimes/covid-19-data/blob/master/us-counties-2022.csv
https://github.com/nytimes/covid-19-data/blob/master/us-counties-2023.csv
The Data folder contains:
•	Initial Estimation of Rt: Using EpiNow2 R package we obtained the initial estimates of Rt.
•	Individual Model Forecasts: Forecast results for Rt were generated using various models, including Random Forest (RF), Regression, and EpiNow2.
•	Ensemble Forecasts: Combined forecasts from multiple models to improve prediction reliability.
•	COVID-19 Case Forecasts: Generated COVID-19 case forecasts based on the Rt forecasts using Poisson stochastic process.
•	Forecast Summary Statistics: Scenario-based summary statistics for COVID-19 cases and Rt forecasts.
3. Figures
The Figures folder contains:
•	Main Figures (Fig. 1, Fig. 2, Fig. 3): Visualizations used in the primary analysis and manuscript.
•	Supplementary Figures (Fig. S1 - Fig. S27): Additional visualizations supporting the analysis.
4. R Code
The R Code folder contains: All the R code used for the analysis are provided.

Forecasting Models and Approaches
The forecasts are generated using:
•	Machine Learning Models: Random Forest, XGBoost, and Regression models for Rt forecasting.
•	Ensemble Modeling: Combining forecasts from different models to improve overall predictive performance.
•	Probabilistic Model: Poisson probabilistic model for generating COVID-19 cases.
Forecasts are produced for:
•	7-day horizon
•	14-day horizon
•	21-day horizon.
Contact:
For questions, please contact:
Md Sakhawat Hossain
Email: mdsakhh@clemson.edu
