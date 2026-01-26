# Clearing the environment
rm(list=ls())
# Set working directory (Update the path if necessary)
setwd("My_directory")

######################################################
#install packages
#install.packages("EpiEstim")
#install.packages("tidyverse")
#install.packages("maps")
#install.packages("zoo")
#install.packages("EpiNow2")
#install.packages("EpiLPS")
# Load necessary libraries
#library(DT)
library(maps)
library(zoo)
library(ggplot2)
library(EpiEstim)
library(EpiNow2)
library(rtestim)
library(EpiLPS)
library(data.table)
# Load necessary libraries
library(dplyr)
library(tidyr)
library(INLA)
library(spdep)
library(sf)
library(tigris)
#########################################################################################################
seed=100
set.seed(seed = seed)


# Read COVID-19 data at the county code level

# Read COVID-19 data at the county level
covid_data_2020 <- read.csv("us-counties-2020.csv")  #load the COVID-19 county level data for the year 2020
covid_data_2021 <- read.csv("us-counties-2021.csv")  #load the COVID-19 county level data for the year 2021
covid_data_2022 <- read.csv("us-counties-2022.csv")  #load the COVID-19 county level data for the year 2022
covid_data_2023 <- read.csv("us-counties-2023.csv")  #load the COVID-19 county level data for the year 2023


covid_data_2020$date<-as.Date(covid_data_2020$date, format = "%m/%d/%Y")  # format the date
covid_data_2021$date<-as.Date(covid_data_2021$date, format = "%m/%d/%Y")  # format the date

covid_data_all<-rbind(covid_data_2020,covid_data_2021,covid_data_2022,covid_data_2023)   # combine the data from 2020 and 2021



# Calculating the rolling average
# we use the 7 days rolling average data

covid_data_all <- covid_data_all %>%
  arrange(county, date) %>%  # Sort by county and date
  group_by(county) %>%  # Group data by county
  mutate(
    cases_7day_avg = rollmean(cases, 7, fill = NA, align = "right"),
    deaths_7day_avg = rollmean(deaths, 7, fill = NA, align = "right")
  )

# Filter the data only for the South Carolina Counties and create a new data frame
# Consider the data only for the period we want to estimate Rt
# the start date of the data should be one day prior to the Rt estimation stat date

covid_data<-covid_data_all %>% 
  filter(state=="South Carolina" & date>="2022-05-01" & date<="2023-02-11") %>%
  dplyr::select(Date=date,County=county,Daily_Cases=cases_7day_avg)

covid_data$Daily_Cases<-round(covid_data$Daily_Cases)    # Since the cases are seven days moving average we need round it to nearest integer for EpiNow2 and EpiEstim

#list of the counties in South Carolina

#SC_counties<-unique(covid_data$County)[-47]

SC_counties<-c("Abbeville","Aiken","Allendale","Anderson","Bamberg","Barnwell","Beaufort","Berkeley","Calhoun","Charleston","Cherokee","Chester",
               "Chesterfield","Clarendon","Colleton","Darlington","Dillon","Dorchester","Edgefield","Fairfield","Florence", "Georgetown",
               "Greenville","Greenwood","Hampton","Horry","Jasper","Kershaw","Lancaster","Laurens","Lee","Lexington","Marion","Marlboro",
               "McCormick","Newberry","Oconee","Orangeburg","Pickens","Richland","Saluda","Spartanburg","Sumter","Union","Williamsburg","York")

# Select the counties we want to Estimate Rt for

#SC_counties<-c("Abbeville","Aiken","Allendale")

covid_data <- covid_data %>%
  filter(County %in% SC_counties)


# # Create an empty data frame to store the results
# results_df <- data.frame(
#   dates = character(0),
#   Rt_EpiNow=numeric(0)
# )

# Define a function for Rt estimation
estimate_Rt <- function(covid_data, start_date, end_date) {
  
  #covid_data$Daily_Cases <- c(NA, diff(covid_data$Cases))
  
  covid_data$Daily_Cases[start_date:end_date] <- ifelse(covid_data$Daily_Cases[start_date:end_date] < 0, 0, covid_data$Daily_Cases[start_date:end_date])
  #covid_data[,6:8] <- rollmean(covid_data[,6:8], k=3, align='right', fill=NA)
  
  if (covid_data$Daily_Cases[start_date] == 0) {
    covid_data$Daily_Cases[start_date] <- 1
  }
  
  
  # Define the data for estimation 
  
  Iday <- covid_data$Daily_Cases[start_date:end_date] 
  dates <- covid_data$Date[start_date:end_date]
  nday <- length(dates)
  
  # Time series lengths
  
  tday <- 1:nday   #define the time points of the estimation period
  mean_si<-4.7       # mean for the serial interval distribution
  std_si<-2.9        #  standard deviation for the serial interval
  
  

  #####################################################
  ## EpiNow2 estimation of effective reproductive number Rt
  #####################################################
  # define the reporting delay distribution
  # generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE)
  # incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max = 10, fixed = TRUE)
  generation_time<-dist_spec(mean =4.7 , sd=2.9, max =10 , distribution = "gamma")
  incubation_period<-dist_spec(mean =5 , sd=2.4 , max =10 , distribution = "gamma")
  reporting_delay <- dist_spec(mean=3.2, sd=1, max=10,distribution = "gamma")
  
  ## data frame for the reported daily cases and corresponding dates
  reported_cases <- data.frame(date=as.Date(dates, format = "%m/%d/%Y"), confirm=Iday)
  
  
  # Estimation of Rt using the epinow function from "EpiNow2" R packages
  n_chains<-2
  n_samples<-2000
  n_warmup<-500
 estimates <- epinow(data  = reported_cases, 
                      generation_time = gt_opts(generation_time), 
                      rt = rt_opts(prior = LogNormal(mean = 1, sd = 0.1)),
                      delays = delay_opts(incubation_period+reporting_delay),
                      stan=EpiNow2::stan_opts(
                        cores=4,
                        chains=n_chains,
                        seed=seed,
                        control=list(
                          adapt_delta=0.99,
                          max_treedepth=12
                        ),
                        samples=n_samples,
                        warmup=n_warmup
                      ),
                      forecast = forecast_opts(horizon = 0)
                      ,
                      CrIs=c(0.95))
   
  Rt_case_dt <- estimates$estimates$summarised
  
  Rt_case_dt[, date := as.Date(date)]
  
  # Find the minimum and maximum dates, excluding NA values
  min_date <- covid_data$Date[start_date]
  max_date <- max(Rt_case_dt$date, na.rm = TRUE)
  # Filter the data for the desired date range
  Rt_case_dt <- Rt_case_dt[date >= min_date & date <= max_date]
  
  Rt_case_dt_wide <- data.table::dcast(Rt_case_dt, date + type ~ variable, value.var = c("median", "lower_95", "upper_95"))
  
  Rt_case_dt_wide_final<-Rt_case_dt_wide %>% dplyr::select(c("date","median_R", "median_reported_cases", 
                                             "lower_95_R", "lower_95_reported_cases", 
                                             "upper_95_R", "upper_95_reported_cases","type"))
  
  Rt_case_dt_wide_final[type == "estimate based on partial data", type := "estimate"]
  
  
  return(Rt_case_dt_wide_final)
}

# Create an empty data frame to store the results
results_df <- data.frame(
  date = character(),
  median_R = numeric(),
  median_reported_cases=numeric(),
  lower_95_R=numeric(),
  lower_95_reported_cases=numeric(),
  upper_95_R=numeric(),
  upper_95_reported_cases=numeric(),
  type=character(),
  County = character()
)


####################################################
###  SC County Level Rt Estimates###################

unique_county=unique(covid_data$County)

for (county in unique_county) {
  # Create the data set for a specific county code
  county_data <- dplyr::filter(covid_data, County == county)
  # Preprocess data
  # Specify the date range for the estimation
  start_date <- which(county_data$Date == "2022-05-01") # day before the Rt estimation start date
  end_date <- which(county_data$Date == "2023-02-11")   # end date of Rt estimation
  
  # Skip if dates are not in the data
  if (length(start_date) == 0 || length(end_date) == 0) {
    next
  }
  
  # if (sum(county_data$Daily_Cases[start_date:end_date])<50) {
  #   next
  # }
  
  # if ((county_data$Daily_Cases[start_date])==0) {
  #   next
  # }
  
  # Estimate Rt for the county code
  county_Rt_estimates <- estimate_Rt(county_data, start_date, end_date)
  
  # Append the county code and combine results
  county_Rt_estimates$County <- county
  results_df <- rbind(results_df, county_Rt_estimates)
}

# View the combined results
head(results_df)
Rt_county<-results_df

##########################




