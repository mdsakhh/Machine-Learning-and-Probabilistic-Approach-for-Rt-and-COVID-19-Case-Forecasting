# Clear workspace and set the working directory
rm(list = ls())
setwd("My_directory")


library(dplyr)
library(Metrics)
library(lubridate)
library(readr)
library(ggplot2)
library(gridExtra)
library(patchwork)

# Set seed for reproducibility
set.seed(100)

forecast_epinow<-read.csv("EpiNow2_forecast_scenario-1_7days_ahead.csv")
forecast_ensemble<-read.csv("Ensemble_forecast_scenario-1_7days_ahead.csv")


forecast_epinow$Date<-as.Date(forecast_epinow$Date, format = "%m/%d/%Y")
forecast_ensemble$Date<-as.Date(forecast_ensemble$Date, format = "%m/%d/%Y")
# forecast_xgboost_smooth$Date<-as.Date(forecast_xgboost_smooth$Date, format = "%m/%d/%Y")
# forecast_regression$Date<-as.Date(forecast_regression$Date, format = "%m/%d/%Y")

# forecast_epinow$Date<-as.Date(forecast_epinow$Date)
#forecast_ensemble$Date<-as.Date(forecast_ensemble$Date)
# forecast_xgboost_smooth$Date<-as.Date(forecast_xgboost_smooth$Date)
# forecast_regression$Date<-as.Date(forecast_regression$Date)

# Function to calculate sMAPE
calculate_sMAPE <- function(actual, forecast) {
  # Ensure inputs are numeric vectors
  actual <- as.numeric(actual)
  forecast <- as.numeric(forecast)
  
  # Calculate sMAPE
  sMAPE <- mean(2 * abs(forecast - actual) / (abs(actual) + abs(forecast)) * 100, na.rm = TRUE)
  return(sMAPE)
}

# Assuming 'forecast_epinow' and 'forecast_xgboost' are your datasets
# Calculate RMSE and sMAPE for Rt and cases
calculate_metrics <- function(data) {
  results <- data %>%
    group_by(County) %>%
    summarise(
      Rt_RMSE = rmse(data$Rt_actual, data$Rt_forecast),
      Rt_sMAPE = calculate_sMAPE(Rt_actual, Rt_forecast),
      Rt_PA=mean(pmin(Rt_actual, Rt_forecast)/pmax(Rt_actual, Rt_forecast))*100,
      Case_RMSE = rmse(Case_actual, Case_forecast),
      Case_sMAPE = calculate_sMAPE(Case_actual, Case_forecast),
      Case_PA=mean((pmin(Case_actual, Case_forecast)/pmax(Case_actual, Case_forecast)),na.rm=TRUE)*100
    )
  return(results)
}

# Apply the function to both datasets
results_epinow <- calculate_metrics(forecast_epinow)
results_ensemmble <- calculate_metrics(forecast_ensemble)
# results_xgboost_smooth <- calculate_metrics(forecast_xgboost_smooth)
# results_regression <- calculate_metrics(forecast_regression)

# Combine results into a single data frame for comparison
final_results <- bind_rows(
  mutate(results_epinow, Model = "EpiNow2"),
  mutate(results_ensemmble, Model = "Ensemble")
)

# Printing the results
print(final_results)

summary_stats <- final_results %>%
  group_by(Model) %>%
  summarise(
    Rt_RMSE_Median = median(Rt_RMSE, na.rm = TRUE),
    Rt_RMSE_Q1 = quantile(Rt_RMSE, 0.25, na.rm = TRUE),
    Rt_RMSE_Q3 = quantile(Rt_RMSE, 0.75, na.rm = TRUE),
    Rt_sMAPE_Median = median(Rt_sMAPE, na.rm = TRUE),
    Rt_sMAPE_Q1 = quantile(Rt_sMAPE, 0.25, na.rm = TRUE),
    Rt_sMAPE_Q3 = quantile(Rt_sMAPE, 0.75, na.rm = TRUE),
    Rt_PA_Median = median(Rt_PA, na.rm = TRUE),
    Rt_PA_Q1 = quantile(Rt_PA, 0.25, na.rm = TRUE),
    Rt_PA_Q3 = quantile(Rt_PA, 0.75, na.rm = TRUE),
    Case_RMSE_Median = median(Case_RMSE, na.rm = TRUE),
    Case_RMSE_Q1 = quantile(Case_RMSE, 0.25, na.rm = TRUE),
    Case_RMSE_Q3 = quantile(Case_RMSE, 0.75, na.rm = TRUE),
    Case_sMAPE_Median = median(Case_sMAPE, na.rm = TRUE),
    Case_sMAPE_Q1 = quantile(Case_sMAPE, 0.25, na.rm = TRUE),
    Case_sMAPE_Q3 = quantile(Case_sMAPE, 0.75, na.rm = TRUE),
    Case_PA_Median = median(Case_PA, na.rm = TRUE),
    Case_PA_Q1 = quantile(Case_PA, 0.25, na.rm = TRUE),
    Case_PA_Q3 = quantile(Case_PA, 0.75, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# View the summary statistics
print(summary_stats)


#########################
# Function to process and calculate weekly metrics for each county
# Function to process and calculate weekly metrics, and ensure county-wise calculations
calculate_weekly_metrics <- function(data) {
  data %>%
    group_by(County, week = floor_date(Date, "week")) %>%
    summarise(
      Weekly_Actual = sum(Case_actual, na.rm = TRUE),
      Weekly_Forecast = sum(Case_forecast, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(County) %>%
    summarise(
      Case_RMSE = sqrt(mean((Weekly_Actual - Weekly_Forecast)^2)),
      Case_sMAPE = mean(abs(Weekly_Actual - Weekly_Forecast) / Weekly_Actual) * 100,
      Case_PA = mean(pmin(Weekly_Actual, Weekly_Forecast) / pmax(Weekly_Actual, Weekly_Forecast)) * 100,
      .groups = "drop"
    )
}

# Apply the function to both datasets
results_epinow_weekly <- calculate_weekly_metrics(forecast_epinow)
results_ensemble_weekly <- calculate_weekly_metrics(forecast_ensemble)
# results_xgboost_smooth_weekly <- calculate_weekly_metrics(forecast_xgboost_smooth)
# results_regression_weekly <- calculate_weekly_metrics(forecast_regression)

# Combine results into a single data frame for comparison
final_results_weekly <- bind_rows(
  mutate(results_epinow_weekly, Model = "EpiNow2"),
  mutate(results_ensemble_weekly, Model = "Ensemble")
)

summary_stats_weekly <- final_results_weekly %>%
  group_by(Model) %>%
  summarise(
    Case_RMSE_Median = median(Case_RMSE, na.rm = TRUE),
    Case_RMSE_Q1 = quantile(Case_RMSE, 0.25, na.rm = TRUE),
    Case_RMSE_Q3 = quantile(Case_RMSE, 0.75, na.rm = TRUE),
    Case_sMAPE_Median = median(Case_sMAPE, na.rm = TRUE),
    Case_sMAPE_Q1 = quantile(Case_sMAPE, 0.25, na.rm = TRUE),
    Case_sMAPE_Q3 = quantile(Case_sMAPE, 0.75, na.rm = TRUE),
    Case_PA_Median = median(Case_PA, na.rm = TRUE),
    Case_PA_Q1 = quantile(Case_PA, 0.25, na.rm = TRUE),
    Case_PA_Q3 = quantile(Case_PA, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

# View the summary statistics
print(summary_stats_weekly)


###############################################
combined_data <- bind_rows(
  forecast_epinow %>% mutate(Model = "EpiNow2"),
  forecast_ensemble %>% mutate(Model = "Ensemble")
) 

# Filter for selected counties
#selected_counties <- c("Greenville", "Charleston", "Horry", "Richland","Lancaster","Spartanburg","York","Berkley")
selected_counties<-unique(combined_data$County)[1:46]
#selected_counties <- c("Greenville", "Charleston", "Cherokee", "Richland","Lancaster","Chesterfield","York")
#selected_counties <- c("Greenville", "Charleston", "Lexington", "Horry","Lancaster","York")
#selected_counties <- c("McCormick", "Saluda", "Edgefield", "Bamberg","Barnwell","Allendale")

combined_data <- combined_data %>% filter(County %in% selected_counties)


# Calculate the maximum and minimum forecast values for each county for Rt
rt_limits <- combined_data %>%
  group_by(County) %>%
  summarise(
    max_rt_forecast = max(Rt_forecast, na.rm = TRUE) ,  # Adding a buffer for the maximum
    min_rt_forecast = min(Rt_forecast, na.rm = TRUE)   # Subtracting for the minimum
  )

# Merge this back into the original data for use in plotting
combined_data <- left_join(combined_data, rt_limits, by = "County")

# Define vertical lines for every 7-day interval across all counties
forecast_intervals <- seq(min(combined_data$Date), max(combined_data$Date), by = 14)
# Rt forecast plot with vertical dashed lines across all counties
rt_plot <- ggplot(combined_data, aes(x = Date)) +
  # Add vertical dashed lines every 7 days across all counties
  geom_vline(xintercept = forecast_intervals, linetype = "dashed", color = "slategray", alpha = 0.8) +
  geom_line(aes(y = Rt_forecast, color = Model, group = interaction(Model, County))) +
  geom_line(aes(y = Rt_actual, color = "Actual"), linetype = "dashed") +  
  facet_wrap(~County, scales = "free_y", nrow = 8,ncol = 6) +
  scale_x_date(date_breaks = "14 day", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0.5,2.0), breaks = seq(0.5,2.0, by = 0.5)) + 
  labs(title = "", y = "Rt Value", x = "Date") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, linewidth =  .6),  # Add border box
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    strip.text = element_text(size=16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  ) +
  scale_color_manual(values = c("Actual" = "black","EpiNow2" = "red", "Ensemble" = "blue"),
                     name = expression(paste(R[t]," Forecast:")),
                     labels = c(expression(hat(R)[t*","*i]),expression(hat(R)[t*","*Ensemble*","*i]^F),expression(hat(R)[t*","*Now*","*i]^F)) # Update this line with your desired labels
)+
  theme(legend.position = "bottom")  # No legend for this plot






# Case forecast plot with a legend


county_limits <- combined_data %>%
  group_by(County) %>%
  summarise(max_forecast = max(Case_forecast, na.rm = TRUE))  # Add a buffer

# Merge this back into the original data for use in plotting
combined_data <- left_join(combined_data, county_limits, by = "County")

# Case forecast plot with vertical dashed lines across all counties
case_plot <- ggplot(combined_data, aes(x = Date)) +
  # Add vertical dashed lines every 7 days across all counties
  geom_vline(xintercept = forecast_intervals, linetype = "dashed", color = "slategray", alpha = 0.8) +
  geom_line(aes(y = Case_forecast, color = Model, group = interaction(Model, County))) +
  geom_line(aes(y = Case_actual, color = "Actual"), linetype = "dashed") +  
  scale_x_date(date_breaks = "14 day", date_labels = "%b %d") +
  facet_wrap(~County, scales = "free_y", nrow = 8,ncol = 6) +
  labs(title = "", y = "Case Count", x = "Date") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, linewidth =  .6),  # Add border box
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    strip.text = element_text(size=16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  ) +
  scale_color_manual(values = c("Actual" = "black","EpiNow2" = "red", "Ensemble" = "blue"),
                     name = expression(paste("Case Forecast:")),
                     labels = c(expression(I[t*","*i]),expression(hat(I)[t*","*Ensemble*","*i]),expression(hat(I)[t*","*Now*","*i])) # Update this line with your desired labels
  )+
  scale_y_continuous(limits = function(x) c(0, max(x) + 1), breaks = function(x) pretty(x, n = 4)) +
  theme(legend.position = "bottom") 

# Print the plot to check it
print(rt_plot)
                     
# Print the plot to check it
print(case_plot)


summary_stats_weekly[,8:10]
summary_stats[,8:10]


