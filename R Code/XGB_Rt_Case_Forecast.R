# Clear workspace and set the working directory
rm(list = ls())
setwd("My_directory")

# Set seed for reproducibility
set.seed(100)

# Load necessary libraries
library(patchwork)
library(tidyr)
library(dplyr)
library(xgboost)
library(Metrics)
library(ggplot2)
library(reshape2)
library(lubridate)
library(zoo)
##############################################################################

# determine the time periods for the training, validation, and test data sets
# # # Wave-1 ------- Wave-2
train_start_date<-as.Date("2022-05-01")
train_end_date<-as.Date("2023-12-10")



# Load the Rt estimation data file
Rt_data<-read.csv("Rt_estimates_initial.csv")
Rt_data$Date<-as.Date(Rt_data$Date, format = "%m/%d/%Y")
Rt_data$day_of_year<-yday(Rt_data$Date)


# Split the data into training, validation, and test sets
Rt_training_data <- Rt_data %>% filter(Date >=train_start_date & Date <=train_end_date)


# Function to create lagged features for all generated time series data
create_lag_features <- function(data, lags, estimation_method) {
  
  lagged_Rt <- embed(data[,  estimation_method], lags + 1)
  lagged_days <- cbind(embed(data[, "day_of_year"], lags + 1)[,1])
  #lagged_days <- embed(data[, "day_of_year"], lags + 1)
  
  colnames(lagged_Rt) <- c("target", paste0("lag_Rt_", 1:lags))
  colnames(lagged_days) <- paste0("lag_day_of_year_", 0)
  #colnames(lagged_days) <- paste0("lag_day_of_year_", 0:lags)
  
  lagged_data <- cbind(as.data.frame(lagged_Rt), as.data.frame(lagged_days))
  return(lagged_data)
}

# Percentage Agreement evaluation metric
pa_eval <- function(preds, data_actual) {
  labels <- getinfo(data_actual, "label")
  pa_value <- mean(pmin(labels, preds) / pmax(labels, preds)) * 100
  return(list(metric = "pa", value = pa_value))  
}

# Define cross-validation function for grid search
cv_xgboost <- function(data_matrix, params, num_folds) {
  cv_results <- xgb.cv(
    params = params,
    data = data_matrix,
    nrounds = 1000,
    nfold = num_folds,
    early_stopping_rounds = 10,
    verbose = 0,
    feval = pa_eval,
    maximize = TRUE
  )
  return(list(best_iteration = cv_results$best_iteration, best_score = max(cv_results$evaluation_log$test_pa_mean)))
}

# Model training function with cross-validation for all hyperparameters
fit_xgboost <- function(train_data, max_lag_values, eta_values, lambda_values, max_depth_values, estimation_method, num_folds) {
  best_params <- list()
  max_pa <- 0
  
  for (lag in max_lag_values) {
    train_data_list <- list()
    for (county in unique(train_data$County)) {
      train_County <- train_data %>% filter(County == county)
      if (nrow(train_County) >= lag) {
        train_lagged_data <- create_lag_features(train_County, lag, estimation_method)
        train_data_list[[county]] <- train_lagged_data
      }
    }
    combined_train_data <- do.call(rbind, train_data_list)
    train_matrix <- xgb.DMatrix(data = as.matrix(combined_train_data[, -1]), label = combined_train_data$target)
    
    for (lmda in lambda_values) {
      for (eta in eta_values) {
        for (max_depth in max_depth_values) {
          params <- list(
            objective = "reg:squarederror",
            eta = eta,
            lambda = lmda,
            max_depth = max_depth,
            subsample = 0.8,
            colsample_bytree = 0.8
          )
          
          cv_results <- cv_xgboost(train_matrix, params,num_folds)
          
          if (cv_results$best_score > max_pa) {
            max_pa <- cv_results$best_score
            best_params <- list(lag = lag, eta = eta, lmda = lmda, max_depth = max_depth, nrounds = cv_results$best_iteration, pa = max_pa)
          }
        }
      }
    }
  }
  return(best_params)
}


estimation_method=c("Rt_EpiNow")

# create the training data for Rt forecasting model fit

training_data<-Rt_training_data %>%
  dplyr::select(Date,all_of(estimation_method),County,day_of_year)

# Define values to test for hyperparameter optimization

max_lag_values <- seq(2, 25, by = 1)  # Test lag values
eta_values <- c(0.05,0.1,0.2,0.3)  # Test different learning rates
max_depth_values <- c(4,6,8,10)  # Test different maximum tree depths
lambda_values<-c(1,2,3,5)  #L2 regularization

# eta_values <- c(0.05)  # Test different learning rates
# max_depth_values <- c(4)  # Test different maximum tree depths
# lambda_values<-c(1)  #L2 regularization


# Run the optimization function
best_params <- fit_xgboost(training_data, max_lag_values, eta_values,lambda_values, max_depth_values, estimation_method,num_folds=5)

# Print the best hyperparameters
cat("Best lag:", best_params$lag, "\n")
cat("Best eta:", best_params$eta, "\n")
cat("Best lambda:", best_params$lmda, "\n")
cat("Best max_depth:", best_params$max_depth, "\n")
cat("Best PA:", best_params$pa, "\n")

optimal_lag<-best_params$lag


#############################################################################################
########################### Forecast#########################################################
# Set up the forecasting interval
# we will perform 7, 14, and 21 days forecast
# Update the optimization function to handle matrices correctly
fit_xgboost_final <- function(train_data, max_lag_values, eta_values,lambda_values, max_depth_values,estimation_method) {
  best_params <- list()
  max_pa <- 0
  
  for (lag in max_lag_values) {
    train_data_list <- list()
    
    for (county in unique(train_data$County)) {
      train_County <- train_data %>% filter(County == county)
      
      if (nrow(train_County) >= lag) {  # Ensure sufficient data for lagging
        train_lagged_data <- create_lag_features(train_County, lag, estimation_method)
        
        train_data_list[[county]] <- train_lagged_data
      }
    }
    
    combined_train_data <- do.call(rbind, train_data_list)
    
    train_matrix <- xgb.DMatrix(data = as.matrix(combined_train_data[, -1]), label = combined_train_data$target)
    
    for (lmda in lambda_values){
      for (eta in eta_values) {
        for (max_depth in max_depth_values) {
          params <- list(
            objective = "reg:squarederror",
            eta = eta,
            lambda=lmda,
            max_depth = max_depth,
            subsample = 0.8,
            colsample_bytree = 0.8
          )
          
          xgb_model <- xgb.train(
            params = params,
            data = train_matrix,
            nrounds = 1000,
            watchlist = list(train = train_matrix),
            early_stopping_rounds = 10,
            verbose = 0,
            feval = pa_eval,  # Use the custom MAPE evaluation function
            maximize = TRUE  # Indicate we want to minimize the MAPE
          )
          
          # Check if the new MAPE score is lower and update best parameters if true
          if (xgb_model$best_score > max_pa) {
            max_pa <- xgb_model$best_score
            best_params <- list(lag = lag, eta = eta,lmda=lmda, max_depth = max_depth, model = xgb_model, pa = max_pa)
          }
        }
      }
    }
  }
  
  return(best_params)
}

#################################

rt_forecast<-function(forecast_start_date,forecast_end_date){
  
  #define the data for the final model fitting
  end_date_final_model<-forecast_start_date-1
  Rt_final_model_data<-Rt_data %>% filter(Date >=train_start_date & Date <=end_date_final_model)
  
  
  final_model_data <- Rt_final_model_data %>%
    dplyr::select(Date, all_of(estimation_method), County, day_of_year)
  
  
  results_final_model<-fit_xgboost_final(final_model_data, optimal_lag, best_params$eta,best_params$lmda, best_params$max_depth, estimation_method)
  final_model<- results_final_model$model
  
  #######################################
  test_start_date<-forecast_start_date-optimal_lag
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
      initial_lags<-rev(c(initial_lags_data$Rt_EpiNow))
      lag_features<-c(initial_lags,yday(forecast_start_date))
      lag_features <- t(as.matrix(lag_features))
      
      forecasts <- numeric(as.integer(forecast_end_date - forecast_start_date) + 1)
      
      # Perform recursive forecasting
      for (i in 1:length(forecasts)) {
        forecast_value <- predict(final_model, newdata = xgb.DMatrix(data = lag_features))
        forecasts[i] <- forecast_value
        
        # Update lags: move everything one step back and insert new forecast at the end
        initial_lags <- c(forecast_value,initial_lags[-length(initial_lags)])
        lag_features<-c(initial_lags,yday(i+forecast_start_date))
        lag_features <- t(as.matrix(lag_features))
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
covid_data_2020 <- read.csv("sc-counties-2020.csv")  #load the COVID-19 county level data for the year 2020
covid_data_2021 <- read.csv("sc-counties-2021.csv")  #load the COVID-19 county level data for the year 2021
covid_data_2022 <- read.csv("sc-counties-2022.csv")  #load the COVID-19 county level data for the year 2022
covid_data_2023 <- read.csv("sc-counties-2023.csv")  #load the COVID-19 county level data for the year 2023


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

######################################################################################

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

# now save the all_forecast_results as csv named as Forecast_XGB


