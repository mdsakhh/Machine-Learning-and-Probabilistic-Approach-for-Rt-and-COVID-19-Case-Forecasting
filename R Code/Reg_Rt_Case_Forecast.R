# Clear workspace and set the working directory
rm(list = ls())
setwd("My_directory")

# Set seed for reproducibility
set.seed(100)

# Load necessary libraries
library(patchwork)
library(tidyr)
library(dplyr)
library(Metrics)
library(ggplot2)
library(reshape2)
library(lubridate)
library(zoo)
library(caret)
##############################################################################

# determine the time periods for the training, validation, and test data sets
# # # Wave-1 ------- Wave-2
train_start_date<-as.Date("2022-05-01")
train_end_date<-as.Date("2022-12-10")


# Load the Rt estimation data file
Rt_data<-read.csv("Rt_estimates_initial.csv")
Rt_data$Date<-as.Date(Rt_data$Date, format = "%m/%d/%Y")
Rt_data$day_of_year<-yday(Rt_data$Date)


# Split the data into training, validation, and test sets
Rt_training_data <- Rt_data %>% filter(Date >=train_start_date & Date <=train_end_date)

# Function to create lagged features for all generated time series data
create_lag_features <- function(data, lags, estimation_method) {
  lagged_Rt <- embed(data[, estimation_method], lags + 1)
  lagged_days <- cbind(embed(data[, "day_of_year"], lags + 1)[,1])
  colnames(lagged_Rt) <- c("target", paste0("lag_Rt_", 1:lags))
  colnames(lagged_days) <- paste0("lag_day_of_year_", 0)
  lagged_data <- cbind(as.data.frame(lagged_Rt), as.data.frame(lagged_days))
  return(lagged_data)
}

# Select the estimation method of Rt for which we would like to do forecast
estimation_method <- c("Rt_EpiNow")


# Train, validation, test data for the selected Rt estimation
training_data <- Rt_training_data %>%
  dplyr::select(Date, all_of(estimation_method), County, day_of_year)



# Fit regression model function
fit_regression_cv <- function( training_data,lags, estimation_method, k,method) {
  train_data_list <- list()
  
  for (county in unique(training_data$County)) {
    train_County <- training_data %>% filter(County == county)
    if (nrow(train_County) >= lags) {
      train_lagged_data <- create_lag_features(train_County, lags, estimation_method)
      train_lagged_data$County <- county
      train_data_list[[county]] <- train_lagged_data
    }
  }
  
  combined_training_data <- do.call(rbind, train_data_list)
  
  # Define k-fold cross-validation
  train_control <- trainControl(method = method, number = k)
  
  # Train linear regression with cross-validation
  model <- train(
    target ~ . - County, 
    data = combined_training_data,
    method = "lm",
    trControl = train_control
  )
  
  # Forecast using validation data
  train_forecast <- predict(model, newdata = combined_training_data)
  combined_training_data$forecast <- train_forecast
  return(list(model = model, data_forecast_train = combined_training_data))
}

# Function to calculate PA for a given lag
evaluate_lag_pa <- function(lags, training_data, estimation_method,k,method) {
  fit_results <- fit_regression_cv(training_data, lags, estimation_method,k,method)
  data_forecast_train <- fit_results$data_forecast_train
  pa <- mean(pmin(data_forecast_train$target, data_forecast_train$forecast) / pmax(data_forecast_train$target, data_forecast_train$forecast)) * 100
  return(pa)
}

# Function to find the best lag using 5-fold CV
find_optimal_lag_cv <- function(lag_range, training_data, estimation_method, k,method) {
  pa_results <- sapply(lag_range, function(lag) {
    evaluate_lag_pa(lag, training_data, estimation_method, k,method)
  })
  
  optimal_lag <- lag_range[which.max(pa_results)]
  return(list(optimal_lag = optimal_lag, pa_results = pa_results))
}

# Define range of lag values to test
lag_range <- seq(2,20, by = 1)

# Perform 5-fold cross-validation to find the best lag
optimal_lag_results <- find_optimal_lag_cv(lag_range, training_data, estimation_method, k = 5,method="cv")
optimal_lag <- optimal_lag_results$optimal_lag
pa_results <- optimal_lag_results$pa_results
pa_results
cat("Optimal Lag Selected:", optimal_lag, "\n")


#############################################################################################
########################### Forecast#########################################################
# Set up the forecasting interval
# we will perform 7, 14, and 21 days forecast

rt_forecast<-function(forecast_start_date,forecast_end_date){
  
  
  #define the data for the final model fitting
  end_date_final_model<-forecast_start_date-1
  Rt_final_model_data<-Rt_data %>% filter(Date >=train_start_date & Date <=end_date_final_model)
  
  final_model_data <- Rt_final_model_data %>%
    dplyr::select(Date, all_of(estimation_method), County, day_of_year)
  
  
  
  # Use optimal lag for forecasting
  results_final_model <- fit_regression_cv(final_model_data, lags = optimal_lag, estimation_method,k=1,method="none")
  final_model <- results_final_model$model
  
  
  ####################################################################################
  test_start_date<-end_date_final_model-optimal_lag
  Rt_test_data <- Rt_data %>% filter(Date >=test_start_date)
  
  test_data <- Rt_test_data %>%
    dplyr::select(Date, all_of(estimation_method), County, day_of_year)
  
  # Initialize an empty data frame to store all results
  all_results <- data.frame(County = character(),
                            Date = as.Date(character()),
                            Rt_actual = numeric(),
                            Rt_forecast = numeric(),
                            stringsAsFactors = FALSE)
  
  # Perform forecasting for each County code
  County_names<-unique(test_data$County)
  
  for (county in County_names) {
    
    # Filter test data for the current County code up to the day before forecasting starts
    # to create the lagged data for forecasting
    County_test_data <- test_data %>% filter(County == county & Date < forecast_start_date)
    
    if (nrow(County_test_data) >= optimal_lag) {
      # Ensure to include 'day_of_year' in the forecast model input
      initial_lags_data <- County_test_data %>% tail(optimal_lag) %>% select(all_of(estimation_method))
      initial_lags<-c(initial_lags_data$Rt_EpiNow)
      lag_features<-c(initial_lags,yday(forecast_start_date))
      lag_features <- as.data.frame(t(lag_features))
      colnames(lag_features) <- c(paste0("lag_Rt_", optimal_lag:1), "lag_day_of_year_0")
      
      # # Add a dummy County column to match the structure expected by the model
      lag_features$County <- county  # Use the current county value
      
      forecasts <- numeric(as.integer(forecast_end_date - forecast_start_date) + 1)
      
      # Perform recursive forecasting
      for (i in 1:length(forecasts)) {
        forecast_value <- predict(final_model, newdata = lag_features)
        forecasts[i] <- forecast_value
        
        # Update lags: move everything one step back and insert new forecast at the end
        initial_lags <- c(initial_lags[-1], forecast_value)
        lag_features<-c(initial_lags,yday(i+forecast_start_date))
        lag_features<- as.data.frame(t(lag_features))
        colnames(lag_features) <- c(paste0("lag_Rt_", optimal_lag:1), "lag_day_of_year_0")
        
        # Add a dummy County column to match the structure expected by the model
        lag_features$County <- county  # Use the current county value
      }
      
      # Prepare results data frame for this County code
      results <- data.frame(
        County = rep(county, length(forecasts)),
        Date = seq(forecast_start_date, forecast_end_date, by = "day"),
        Rt_actual = Rt_data %>% filter(County == county & Date >= forecast_start_date & Date <= forecast_end_date) %>% pull(all_of(estimation_method)),
        Rt_forecast = forecasts
      )
      
      # Append results to the all_results data frame
      all_results <- rbind(all_results, results)
    } else {
      cat("Not enough historical data to create lagged features for County", County_names, ". Needed:", best_params$lag, "Got:", nrow(County_test_data), "\n")
    }
  }
  
  return(all_results)
}


##############################################################################################################
##############################################################################################################

start_date<-"2022-12-11"
end_date<-"2023-03-04"

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
  filter(state=="South Carolina" & Date>=train_start_date & Date<=end_date) %>%
  dplyr::select(Date=Date,County=county,Daily_Cases=cases_7day_avg)

covid_data$Daily_Cases<-round(covid_data$Daily_Cases)    # Since the cases are seven days moving average we need round it to nearest integer for EpiNow2 and EpiEstim



SC_counties<- unique(training_data$County)


covid_data_SC <- covid_data %>%
  filter(County %in% SC_counties)

###################################################################################
###################################################################################
set.seed(100)
# Given mean, standard deviation, and max value
mean_val <- 4.7
sd_val <- 2.9
max_val <- 14
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

##################################################################################

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


forecast_step<-7

# Define the start and end Date for the forecasting period
overall_start_date <- as.Date("2022-12-11")
overall_end_date <- as.Date("2023-03-04")

# Initialize an empty data frame to store the results
all_forecast_results <- data.frame()

# Iterate through the forecasting periods in increments of 5 days
current_start_date <- overall_start_date

while (current_start_date <= overall_end_date) {
  
  # Define the forecast period for this iteration
  forecast_start_date <- current_start_date
  forecast_end_date <- min(current_start_date + (forecast_step-1), overall_end_date)
  
  # Perform the forecast for the current period
  rt_forecast_results <- rt_forecast(forecast_start_date, forecast_end_date)
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



