# Machine-Learning-and-Probabilistic-Approach-for-Rt-and-COVID-19-Case-Forecasting
Machine Learning and Probabilistic Approach for Rt and COVID-19 Case Forecasting
Contents
1. Data
The Data folder contains:
•	County Level COVID-19 Case Data: COVID-19 daily case data at the county level in South Carolina (SC) was used for the analysis.
•	Initial Estimation of Rt: Using EpiNow2 R package we obtained the initial estimates of Rt.
•	Individual Model Forecasts: Forecast results for Rt was generated using various models, including Random Forest (RF), Regression, and EpiNow2.
•	Ensemble Forecasts: Combined forecasts from multiple models to improve prediction reliability.
•	COVID-19 Case Forecasts: Generated COVID-19 case forecasts based on the Rt forecasts using Poisson stochastic process.
•	Forecast Summary Statistics: Scenario-based summary statistics for COVID-19 cases and Rt forecasts.
2. Figures
The Figures folder contains:
•	Main Figures (Fig. 1, Fig. 2, Fig. 3): Visualizations used in the primary analysis and manuscript.
•	Supplementary Figures (Fig. S1 - Fig. S26): Additional visualizations supporting the analysis.
3. R Code
The R Code folder contains: All the R code used for the analysis are provided.

Forecasting Models and Approaches
The forecasts are generated using:
•	Machine Learning Models: Random Forest, XGBoost, and Regression models for Rt forecasting.
•	Ensemble Modeling: Combining forecasts from different models to improve overall predictive performance.
•	Probabilistic Model: Poisson probabilistic model for generating COVID-19 cases.
Forecasts are produced for:
•	7-day ahead
•	14-day ahead
•	21-day ahead
Contact
For questions, please contact:
Md Sakhawat Hossain
Email: mdsakhh@clemson.edu
