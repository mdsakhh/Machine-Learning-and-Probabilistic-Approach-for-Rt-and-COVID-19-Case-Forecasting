# Clear workspace and set the working directory
rm(list = ls())
setwd("My_directory")

# Set seed for reproducibility
set.seed(100)

# Load necessary libraries
library(randomForest)
library(caret)  
library(tidyr)
library(dplyr)
library(Metrics)
library(ggplot2)
library(reshape2)
library(lubridate)
library(EpiNow2)
library(zoo)
##############################################################################

# Determine the time periods for training, validation, and test data sets
train_start_date <- as.Date("2020-06-01")
train_end_date <- as.Date("2020-11-10")

# Load the Rt estimation data file
Rt_data <- read.csv("Rt_estimates_initial.csv")
Rt_data$Date <- as.Date(Rt_data$Date, format = "%m/%d/%Y")
Rt_data$day_of_year <- yday(Rt_data$Date)

# Split the data into training and test sets
Rt_training_data <- Rt_data %>% filter(Date >= train_start_date & Date <= train_end_date)

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
  #labels <- getinfo(data_actual, "label")
  pa_value <- mean(pmin(data_actual, preds) / pmax(data_actual, preds)) * 100
  return(list(metric = "pa", value = pa_value))  
}

# Function to optimize Random Forest hyperparameters
fit_random_forest <- function(train_data, max_lag_values, n_tree_values, max_node_values,node_size_values, estimation_method,num_folds,method) {
  
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
    
    for (n_tree in n_tree_values) {
      for (max_node in max_node_values) {
        for (node_size in node_size_values) {
        
        # Define 5-fold cross-validation control
        train_control <- trainControl(method = method, number = num_folds) 
        
        # Train the model using caret's built-in 5-fold cross-validation
        rf_model <- train(
          target ~ ., 
          data = combined_train_data,
          method = "rf",
          trControl = train_control,
          ntree = n_tree,
          maxnodes=max_node,
          nodesize=node_size
        )
        
        # Get best PA from CV results
        preds <- predict(rf_model, combined_train_data)
        pa <- pa_eval(preds, combined_train_data$target)$value
        
        if (pa > max_pa) {
          max_pa <- pa
          best_params <- list(lag = lag, n_tree = n_tree, max_node = max_node,node_size=node_size, model = rf_model, pa = max_pa)
        }
      }
    }
  }
}
  
  return(best_params)
}


# Define values to test for hyperparameter optimization
max_lag_values <- seq(10, 25, by = 1)  # Test lag values
# n_tree_values <- c(50,100,150,200,250,300,350,400,450,500)  # Test different numbers of trees
# max_node_values <- c(50,100,150,200,250,300,350,400,450,500)  # Test different maximum tree nodes
n_tree_values <- c(100,200,300,400,500)  # Test different numbers of trees
max_node_values <- c(50,100,150)  # Test different maximum tree nodes
node_size_values <- c(5,8,10)  # Test different maximum tree nodes




# Specify the estimation method
estimation_method <- "Rt_EpiNow"

# Create the training data for Rt forecasting
training_data <- Rt_training_data %>%
  dplyr::select(Date, all_of(estimation_method), County, day_of_year)

# Run the optimization function
best_params <- fit_random_forest(training_data, max_lag_values, n_tree_values, max_node_values,node_size_values, estimation_method,num_folds=5,method = "cv")

# Print the best hyperparameters
cat("Best lag:", best_params$lag, "\n")
cat("Best n_tree:", best_params$n_tree, "\n")
cat("Best max_node:", best_params$max_node, "\n")
cat("Best node_size:", best_params$node_size, "\n")
cat("Best PA:", best_params$pa, "\n")

optimal_lag <- best_params$lag

#############################################################################################
########################### Forecast ########################################################
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
  
  
  results_final_model<-fit_random_forest(final_model_data, optimal_lag, best_params$n_tree, best_params$max_node, best_params$node_size,estimation_method,num_folds=1,method="none")
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
    County_test_data <- test_data %>% filter(County == county & Date <= forecast_start_date)
    
    if (nrow(County_test_data) >= optimal_lag) {
      # Ensure to include 'day_of_year' in the forecast model input
      initial_lags_data <- County_test_data %>% tail(optimal_lag+1) %>% select(all_of(estimation_method), day_of_year)
      initial_lags <- c(head(initial_lags_data[,1], -1), tail(initial_lags_data$day_of_year, 1))
      #initial_lags <- c(head(initial_lags_data[,1], -1), tail(initial_lags_data$day_of_year))
      initial_lags <- matrix(initial_lags,nrow=1)
      
      forecasts <- numeric(as.integer(forecast_end_date - forecast_start_date) + 1)
      
      # Perform recursive forecasting
      for (i in 1:length(forecasts)) {
        colnames(initial_lags) <- c(paste0("lag_Rt_", optimal_lag:1), "lag_day_of_year_0")
        forecast_value <- predict(final_model, newdata =  initial_lags)
        forecasts[i] <- forecast_value
        
        # Update lags: move everything one step back and insert new forecast at the end
        initial_lags_data <- rbind(initial_lags_data[-1, ], c(forecast_value, yday(forecast_start_date + i)))
        initial_lags <- c(head(initial_lags_data[,1], -1), tail(initial_lags_data$day_of_year, 1))
        initial_lags <- matrix(initial_lags,nrow=1)
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
# mean_val <- 4.7
# sd_val <- 2.9
# max_val <- 10
# 
# # Calculate the shape and rate parameters for the gamma distribution
# shape_param <- (mean_val^2) / (sd_val^2)
# rate_param <- mean_val / (sd_val^2)

# geenrationn distribution using the gamma pdf
#generation_dist<-dist_spec(mean =4.7 , sd=2.9, max =10 , distribution = "gamma")
generation_dist<-c(0.04, 0.13, 0.17, 0.17, 0.15, 0.12, 0.088, 0.063, 0.044, 0.03)

# Recursive case forecasting function for each County code
recursive_forecast <- function(historical_cases, rt_forecasts, generation_dist, forecast_start_date) {
  # Initialize an empty vector for the forecasted cases
  forecasted_cases <- numeric(length(rt_forecasts$Date))
  lower_bound <- numeric(length(rt_forecasts$Date))
  upper_bound <- numeric(length(rt_forecasts$Date))
  
  
  forecast_idx<-which(historical_cases$Date==forecast_start_date)
  historical_case<-historical_cases$Daily_Cases[1:(forecast_idx-1)]
  #historical_case<-historical_cases$Daily_Cases
  # Loop through each Rt forecast date
  for (i in seq_along(rt_forecasts$Date)) {
    Rt <- rt_forecasts$Rt_forecast[i]
    
    # Update historical cases with the forecasted case from the previous step
    if (i > 1) {
      historical_case <- c(historical_case, forecasted_cases[i-1])
    }
    
    # Calculate the infectious value (lambda) using past historical cases and generation_dist
    infectious <- sum(historical_case[(forecast_idx-11+i):(forecast_idx+i-2)]*generation_dist)
    lambda <- Rt * infectious
    
    # Forecast the number of cases using a Poisson distribution
    forecasted_cases[i] <- round(mean(rpois(1000, lambda),na.rm=TRUE))
    lower_bound[i] <- round(quantile(rpois(1000, lambda), 0.025,na.rm=TRUE))  # 2.5th percentile
    upper_bound[i] <- round(quantile(rpois(1000, lambda), 0.975,na.rm=TRUE))  # 97.5th percentile
  }
  
  return(data.frame(
    Date = rt_forecasts$Date,
    Case_forecast = forecasted_cases,
    Case_lower95 = lower_bound,
    Case_upper95 = upper_bound
  ))
}

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

