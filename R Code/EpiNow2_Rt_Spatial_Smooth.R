# Clearing the environment
rm(list=ls())
setwd("My_directory")

######################################################
#install packages
#install.packages("EpiEstim")
#install.packages("tidyverse")
#install.packages("maps")
#install.packages("zoo")
#install.packages("EpiNow2")
# Load necessary libraries
#library(DT)
library(maps)
library(zoo)
library(ggplot2)
library(EpiEstim)
library(EpiNow2)
# Load necessary libraries
library(dplyr)
library(tidyr)
library(INLA)
library(spdep)
library(sf)
library(tigris)
#########################################################################################################

county_level_data<-read.csv("SC_county_sociodemographic_data.csv")

county_level_data$Scaled_Median.Income<-county_level_data$Median_Income/max(county_level_data$Median_Income)

####################################################################################
# Read in the Rt estimates for South Carolina
Rt_county <- read.csv("Rt_estimates_initial.csv")

# Renaming 'County' to 'NAME'
county_level_data <- county_level_data %>% 
  rename(NAME = County)

Rt_county<-Rt_county %>% 
  rename(NAME = County)

#####################################################################################

# load the shape file for the South Carolina counties
# Set options to use a more detailed, larger size shapefile
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Download shapefile for counties in South Carolina (FIPS code for SC is 45) and Texas (FIPS code for TX is 48)
shape_counties <- counties(state = "45", cb = TRUE)
# Create the adjacency matrix as before
neighbors <- poly2nb(shape_counties)
adjacency_matrix <- nb2mat(neighbors, style = "B", zero.policy = TRUE)

shape_counties <- st_as_sf(shape_counties)


# Define a function to fit the Besag model and extract predictions
fit_and_predict <- function(response_var) {
  
  
  ####################################################################
  # Initialize a data frame to store the results
  fitted_values_by_date <- data.frame(Date = character(), County = character(), Fitted_Rt = numeric())
  # Loop through each date in the Rt dataset
  unique_dates <- unique(Rt_county$Date)
  
  for(date in unique_dates) {
    print(date)
    # Subset the Rt data for the current date
    Rt_current <- subset(Rt_county, Date == date)
    
    # Merge this Rt data with your spatial and demographic data
    # Ensure that the merging is done correctly based on the county names and any other relevant keys
    data_model <- merge(Rt_current, county_level_data, by = "NAME")  # Modify as needed
    data_spatial <- left_join(data_model, shape_counties, by = "NAME")  # Adjust the key as necessary
    
    
    # Create an index variable as before
    data_spatial$index <- as.numeric(factor(data_spatial$NAME))
    
    
    # Define the Besag model formula using the Rt data for the current date
    besag_formula <- as.formula(paste(response_var, "~ Percent_20_44 + Percent_45_64 + 
      Percent_Age_65_years_and_over + SVI +
      Percent.Insured + Percent_Black_African.American +
      Percent_Other + Percent_Hispanic +
      Percent_Male + Percent_Labor.Force +
      Percent_Employed + Scaled_Median.Income +
      f(index, model = 'besag', graph = adjacency_matrix)"))
    
    # Fit the Besag model using INLA for the current date
    besag_spatial_bayesian_model <- inla(besag_formula, data = data_spatial, family = "gaussian")
    
    current_fitted <- besag_spatial_bayesian_model$summary.fitted.values$mean
    # Create a data frame for the current date
    current_df <- data.frame(Date = date, 
                             County = levels(factor(data_spatial$NAME)), 
                             Fitted_Rt = current_fitted)
    
    # Append to the main data frame
    fitted_values_by_date <- rbind(fitted_values_by_date, current_df)
  }
  #sorted_fitted_values_by_date <- fitted_values_by_date[order(fitted_values_by_date$County), ]
  return(fitted_values_by_date)
}




####################################################################





predicted_Rt <- fit_and_predict("Rt_forecast")
predicted_case<-fit_and_predict("Case_forecast")



Rt_county$Rt_forecast<-predicted_Rt$Fitted_Rt
Rt_county$Case_forecast<-predicted_case$Fitted_Rt


write.csv(Rt_county,"EpiNow2_Rt_estimates_spatial_smooth.csv")

