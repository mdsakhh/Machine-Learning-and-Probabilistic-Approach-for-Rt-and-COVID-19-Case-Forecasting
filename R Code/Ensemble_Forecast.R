# This code is for ensemble forecast of the effective reproductive number (Rt)
# Clear workspace and set the working directory
rm(list = ls())
setwd("my_directory")


library(dplyr)
library(Metrics)
library(lubridate)
library(readr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(zoo)

# Set seed for reproducibility
set.seed(100)

# Load the Rt forecasts data from six different model

forecast_xgboost<-read.csv("Forecast_XGB.csv")
forecast_xgboost_smooth<-read.csv("Forecast_XGB_Smooth.csv")
forecast_RF<-read.csv("Forecast_RF.csv")
forecast_RF_smooth<-read.csv("Forecast_RF_Smooth.csv")
forecast_regression<-read.csv("Forecast_Reg.csv")
forecast_regression_smooth<-read.csv("Forecast_Reg_Smooth.csv")

#Convert the date into month/day/year format
#use one of the following conversion steps (after conversion, ensure the Date format is correct, if not load the data again and use the second approach for conversion)

forecast_xgboost$Date<-as.Date(forecast_xgboost$Date, format = "%m/%d/%Y")
forecast_xgboost_smooth$Date<-as.Date(forecast_xgboost_smooth$Date, format = "%m/%d/%Y")
forecast_RF$Date<-as.Date(forecast_RF$Date, format = "%m/%d/%Y")
forecast_RF_smooth$Date<-as.Date(forecast_RF_smooth$Date, format = "%m/%d/%Y")
forecast_regression$Date<-as.Date(forecast_regression$Date, format = "%m/%d/%Y")
forecast_regression_smooth$Date<-as.Date(forecast_regression_smooth$Date, format = "%m/%d/%Y")


# forecast_xgboost$Date<-as.Date(forecast_xgboost$Date)
# forecast_xgboost_smooth$Date<-as.Date(forecast_xgboost_smooth$Date)
# forecast_RF$Date<-as.Date(forecast_RF$Date)
# forecast_RF_smooth$Date<-as.Date(forecast_RF_smooth$Date)
# forecast_regression$Date<-as.Date(forecast_regression$Date)
# forecast_regression_smooth$Date<-as.Date(forecast_regression_smooth$Date)

# We use the percenatge agreement to calculate the weights for ensemble method
# the following function is for PA calcualtion
calculate_PA <- function(data) {
  # Calculate PA
  data_PA <- data %>%
    group_by(County,Date) %>%
    mutate(
      PA = min(Rt_actual, Rt_forecast) / max(Rt_actual, Rt_forecast),
    )
  return(data_PA)
}



# Apply the function to calculate PA

results_xgboost <- calculate_PA(forecast_xgboost)
results_xgboost_smooth <- calculate_PA(forecast_xgboost_smooth)
results_RF <- calculate_PA(forecast_RF)
results_RF_smooth <- calculate_PA(forecast_RF_smooth)
results_regression <- calculate_PA(forecast_regression)
results_regression_smooth <- calculate_PA(forecast_regression_smooth)

#Ensemble method's weight calcualtion
sum_PA<-rowSums(cbind(results_xgboost$PA,results_xgboost_smooth$PA,results_RF$PA, results_RF_smooth$PA,results_regression$PA,results_regression_smooth$PA))
results_xgboost$Weight<-results_xgboost$PA/sum_PA
results_xgboost_smooth$Weight<-results_xgboost_smooth$PA/sum_PA
results_RF$Weight<-results_RF$PA/sum_PA
results_RF_smooth$Weight<-results_RF_smooth$PA/sum_PA
results_regression$Weight<-results_regression$PA/sum_PA
results_regression_smooth$Weight<-results_regression_smooth$PA/sum_PA

# Ensemmble forecast of Rt

forecast_ensemble<-results_xgboost$Weight*results_xgboost$Rt_forecast+results_xgboost_smooth$Weight*results_xgboost_smooth$Rt_forecast+results_RF$Weight*results_RF$Rt_forecast+results_RF_smooth$Weight*results_RF_smooth$Rt_forecast+results_regression$Weight*results_regression$Rt_forecast+results_regression_smooth$Weight*results_regression_smooth$Rt_forecast

Rt_forecast_ensemble<-results_xgboost_smooth%>%
  select(County,Date, Rt_actual)
Rt_forecast_ensemble$Rt_forecast<-forecast_ensemble


#######################################################################
##############################################################################################################
##############################################################################################################

# Now based on the Rt forecast we will generate the Case forecast utilizing the historical data
# The following steps are for the Case forecast generation
# Determine the time periods for training, validation, and test data sets

train_start_date <- as.Date("2020-06-01")
train_end_date <- as.Date("2020-11-10")

start_date<-"2020-11-11"
end_date<-"2021-02-02"

forecast_start_date <- as.Date(start_date)
forecast_end_date <- as.Date(end_date) 

##############
# Read COVID-19 data at the county level
covid_data_2020 <- read.csv("us-counties_rolling-average-2020.csv")  #load the COVID-19 county level data for the year 2020
covid_data_2021 <- read.csv("us-counties_rolling-average-2021.csv")  #load the COVID-19 county level data for the year 2021
covid_data_2022 <- read.csv("us-counties_rolling-average-2022.csv")  #load the COVID-19 county level data for the year 2022
covid_data_2023 <- read.csv("us-counties_rolling-average-2023.csv")  #load the COVID-19 county level data for the year 2023


covid_data_2020$Date<-as.Date(covid_data_2020$date, format = "%m/%d/%Y")  # format the date
covid_data_2021$Date<-as.Date(covid_data_2021$date, format = "%m/%d/%Y")  # format the date
covid_data_2022$Date<-covid_data_2022$date
covid_data_2023$Date<-covid_data_2023$date

covid_data_all<-rbind(covid_data_2020,covid_data_2021,covid_data_2022,covid_data_2023)   # combine the data from 2020 and 2021



# Calculating the rolling average
# we use the 7 days rolling average data

covid_data_all <- covid_data_all %>%
  arrange(county, Date) %>%  # Sort by county and Date
  group_by(county) %>%  # Group data by county
  dplyr::mutate(
    cases_7day_avg = rollmean(cases, 7, fill = NA, align = "right"),
    deaths_7day_avg = rollmean(deaths, 7, fill = NA, align = "right")
  )

# Filter the data only for the South Carolina Counties and create a new data frame
# Consider the data only for the period we want to estimate Rt
# the start Date of the data should be one day prior to the Rt estimation stat Date

covid_data<-covid_data_all %>% 
  dplyr::filter(state=="South Carolina" & Date>=train_start_date & Date<=end_date) %>%
  dplyr::select(Date=Date,County=county,Daily_Cases=cases_7day_avg)

covid_data$Daily_Cases<-round(covid_data$Daily_Cases)    # Since the cases are seven days moving average we need round it to nearest integer for EpiNow2 and EpiEstim



SC_counties<- unique(Rt_forecast_ensemble$County)


covid_data_SC <- covid_data %>%
  filter(County %in% SC_counties)

###################################################################################
###################################################################################


###################################################################################
set.seed(100)
# Parameters for the gamma distribution (serial interval)
# Given mean, standard deviation, and max value
mean_val <- 4.7
sd_val <- 2.9
max_val <- 10
# 
# Calculate the shape and rate parameters for the gamma distribution
shape_param <- (mean_val^2) / (sd_val^2)
rate_param <- mean_val / (sd_val^2)

# days
days<-1:max_val

# Gamma PMF approximation using difference of CDFs (discretized PDF)
generation_dist <- pgamma(days, shape = shape_param, rate = rate_param) - 
  pgamma(days - 1, shape = shape_param, rate = rate_param)

# Normalize to ensure it sums to 1 (sometimes due to tail approximation, slight adjustment is needed)
generation_dist <- rev(generation_dist / sum(generation_dist))

# Recursive case forecasting function with full path simulation
recursive_forecast <- function(historical_cases, rt_forecasts, generation_dist, forecast_start_date, n_sim = 1000) {
  # number of forecast steps
  n_steps <- length(rt_forecasts$Date)
  
  # Matrix to store all simulation paths
  sim_cases <- matrix(0, nrow = n_steps, ncol = n_sim)
  
  # Initial historical cases up to the forecast start date
  forecast_idx <- which(historical_cases$Date == (forecast_start_date - 1)) + 1
  historical_case <- historical_cases$Daily_Cases[1:(forecast_idx-1)]
  
  # Run n_sim independent simulations
  for (sim in 1:n_sim) {
    hist_cases_sim <- historical_case
    
    for (i in seq_len(n_steps)) {
      Rt <- rt_forecasts$Rt_forecast[i]
      
      # Infectiousness from past cases
      infectious <- sum(tail(hist_cases_sim, max_val) * generation_dist)
      lambda <- Rt * infectious
      
      # Draw one Poisson sample for this path
      new_case <- rpois(1, lambda)
      
      # Store forecasted case for this trajectory
      sim_cases[i, sim] <- new_case
      
      # Update historical cases for the next step
      hist_cases_sim <- c(hist_cases_sim, new_case)
    }
  }
  
  # Summarize across simulations: mean and 95% CI
  forecasted_cases <- rowMeans(sim_cases)
  lower_bound <- apply(sim_cases, 1, quantile, probs = 0.025)
  upper_bound <- apply(sim_cases, 1, quantile, probs = 0.975)
  
  return(data.frame(
    Date = rt_forecasts$Date,
    Case_forecast = forecasted_cases,
    Case_lower95 = lower_bound,
    Case_upper95 = upper_bound
  ))
}
###############################################################

# Filter historical cases based on the provided date range
actual_case_data_for_forecast <- covid_data_SC %>% 
  dplyr::filter(Date >= train_start_date)

# Updated case forecast function to include confidence intervals
case_forecast <- function(forecast_start_date, forecast_end_date) {
  forcasted_cases_all_list <- list()
  
  # Loop over each County and calculate forecasts
  for (i in seq_along(SC_counties)) {
    County <- SC_counties[i]
    hist_cases_County <- actual_case_data_for_forecast[actual_case_data_for_forecast$County == County,]
    rt_forecasts_County <- rt_forecast_results[rt_forecast_results$County == County, ]
    
    # Forecast with confidence intervals
    forecasted_cases_County <- recursive_forecast(hist_cases_County, rt_forecasts_County, generation_dist, forecast_start_date)
    
    # Add County information
    forecasted_cases_County$County <- County
    forcasted_cases_all_list[[i]] <- forecasted_cases_County
  }
  
  # Combine all results
  forcasted_cases_all_combine <- do.call(rbind, forcasted_cases_all_list)
  return(forcasted_cases_all_combine)
}


forecast_step<-7  # set number of day ahead forecast (7-day, 14-day, 21-day)

# Define the start and end Date for the forecasting period
overall_start_date <- as.Date("2020-11-11")
overall_end_date <- as.Date("2021-02-02")

# Initialize an empty data frame to store the results
all_forecast_results <- data.frame()

# Iterate through the forecasting periods in increments of 5 days
current_start_date <- overall_start_date

while (current_start_date <= overall_end_date) {
  
  # Define the forecast period for this iteration
  forecast_start_date <- current_start_date
  forecast_end_date <- min(current_start_date + (forecast_step-1), overall_end_date)
  
  # Perform the forecast for the current period
  rt_forecast_results <-Rt_forecast_ensemble %>% dplyr::filter(Date>=forecast_start_date & Date<=forecast_end_date)
  case_forecast_all <- case_forecast(forecast_start_date, forecast_end_date)
  
  # Filter actual case data for the current period
  case_actual_data <- covid_data_SC %>% 
    filter(Date >= forecast_start_date & Date <= forecast_end_date) %>%
    select(Date = Date, County = County, Case_actual = Daily_Cases)
  
  # Update the Rt_case_forecast_results to include confidence intervals
  Rt_case_forecast_results <- rt_forecast_results
  Rt_case_forecast_results$Case_forecast <- case_forecast_all$Case_forecast
  Rt_case_forecast_results$Case_lower95 <- case_forecast_all$Case_lower95
  Rt_case_forecast_results$Case_upper95 <- case_forecast_all$Case_upper95
  Rt_case_forecast_results$Case_actual <- case_actual_data$Case_actual
  
  # Append the results to the cumulative data frame
  all_forecast_results <- bind_rows(all_forecast_results, Rt_case_forecast_results)
  
  # Move the start date forward by 5 days
  current_start_date <- current_start_date + forecast_step
}

# View the combined forecast results
print(all_forecast_results)

# save Rt and COVID-19 case forecast data in csv file

#write.csv(all_forecast_results,"Ensemble_forecast.csv")


