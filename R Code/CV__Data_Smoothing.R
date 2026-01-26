# ============================================================
# UPDATED CV (Option B): Hold out last 20% (time-based) + 1-day ahead forecast
# Forecast method (Option B): 1-step-ahead prediction = last available smoothed value
#   pred(t) = smoothed(t-1)  (computed using only data up to t-1)
#
# PURPOSE: Compare smoothing windows (3,5,7,10,14) as preprocessing
# DATA: NYT county daily cases (SC only), missing dates -> 0, negatives -> 0
# SCENARIOS:
#   Scenario-1: 2020-06-01 to 2021-02-02
#   Scenario-2: 2022-05-01 to 2023-03-04
#
# REPORT ONLY: MAE + RMSE
# OUTPUTS:
#   - Tables: Summary + by-county (MAE/RMSE only)
#   - Figure: TWO stacked plots (MAE on top, RMSE below)
#       Bars = mean across counties
#       Point = median across counties
#       Error bars = IQR (Q1–Q3)
#       No titles (use caption)
#       Legend labels: Scenario-1, Scenario-2
#       Y labels: Error magnitude (MAE) / Error magnitude (RMSE)
#   - Save figure as .png/.pdf/.svg
# ============================================================

# -----------------------------
# Libraries
# -----------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(svglite)
library(patchwork)

# -----------------------------
# Set working directory
# -----------------------------
setwd("C:/Users/mdsakhh/Box/BoxPHI-PHMR Projects/Sakhawat/Manuscripts/Epidemics_Submission/github_data_code")

# -----------------------------
# Helper: robust date parsing
# -----------------------------
parse_date_safe <- function(x) {
  out <- suppressWarnings(mdy(x))
  out2 <- suppressWarnings(ymd(x))
  as.Date(ifelse(!is.na(out), out, out2))
}

# -----------------------------
# Load and prepare data (SC only)
# -----------------------------
load_and_prepare_data <- function(year) {
  filename <- paste0("us-counties_rolling-average-", year, ".csv")
  
  read_csv(filename, show_col_types = FALSE) %>%
    dplyr::mutate(date = parse_date_safe(date)) %>%
    dplyr::filter(state == "South Carolina") %>%
    dplyr::filter(county != "Unknown") %>%
    dplyr::mutate(
      cases = as.numeric(cases),
      cases = ifelse(is.na(cases), 0, cases),
      cases = ifelse(cases < 0, 0, cases)
    )
}

# -----------------------------
# Complete missing dates (per county) and impute with 0
# -----------------------------
complete_county_dates <- function(df) {
  df %>%
    dplyr::group_by(geoid, county, state) %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    tidyr::fill(county, state, .direction = "downup") %>%
    dplyr::mutate(
      cases = ifelse(is.na(cases), 0, cases),
      cases = ifelse(cases < 0, 0, cases)
    ) %>%
    dplyr::ungroup()
}

# -----------------------------
# Rolling average (right-aligned)
# -----------------------------
calc_rolling_avg <- function(x, window) {
  zoo::rollmean(x, k = window, fill = NA, align = "right")
}

# -----------------------------
# Option B: 20% holdout + 1-step ahead using last smoothed value
# pred(t) = smoothed(t-1)
# Evaluate vs RAW observed cases at day t
# -----------------------------
holdout_cv_county_1step_lastsmoothed <- function(county_data,
                                                 window_size,
                                                 holdout_frac = 0.20,
                                                 min_train = 60) {
  
  county_data <- county_data %>% dplyr::arrange(date)
  
  raw_cases <- county_data$cases
  smoothed  <- calc_rolling_avg(raw_cases, window_size)
  
  n <- length(raw_cases)
  if (n < (min_train + 10)) return(NULL)
  
  test_n <- max(1, floor(n * holdout_frac))
  test_start <- n - test_n + 1
  if (test_start <= min_train) return(NULL)
  
  out_list <- vector("list", test_n)
  j <- 0
  
  for (idx in test_start:n) {
    prev_idx <- idx - 1
    if (prev_idx < 1) next
    
    pred <- smoothed[prev_idx]
    if (is.na(pred)) next
    pred <- max(pred, 0)
    
    actual <- raw_cases[idx]
    if (is.na(actual)) next
    
    j <- j + 1
    out_list[[j]] <- tibble(
      date = county_data$date[idx],
      split = "holdout20",
      horizon = 1,
      actual_raw = actual,
      predicted = pred,
      abs_error = abs(actual - pred),
      sq_error  = (actual - pred)^2
    )
  }
  
  if (j == 0) return(NULL)
  dplyr::bind_rows(out_list[1:j])
}

# -----------------------------
# Evaluate all windows for one scenario (across counties)
# -----------------------------
evaluate_window_holdout <- function(data,
                                    scenario_name,
                                    start_date,
                                    end_date,
                                    window_sizes = c(3, 5, 7, 10, 14),
                                    holdout_frac = 0.20,
                                    min_train = 60) {
  
  scenario_data <- data %>%
    dplyr::filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    dplyr::arrange(geoid, date)
  
  cat(sprintf("\n=== %s ===\n", scenario_name))
  cat(sprintf("Date range: %s to %s\n", start_date, end_date))
  cat(sprintf("Counties: %d\n", dplyr::n_distinct(scenario_data$geoid)))
  cat(sprintf("CV split: last %.0f%% holdout, 1-day ahead\n", holdout_frac * 100))
  cat("Forecast rule (Option B): pred(t) = smoothed(t-1)\n")
  cat("Evaluation: compared against raw (unsmoothed) observed cases\n")
  
  results <- list()
  
  for (window in window_sizes) {
    cat(sprintf("  Window = %d days\n", window))
    
    county_details <- scenario_data %>%
      dplyr::group_by(geoid) %>%
      dplyr::group_split() %>%
      lapply(function(county_df) {
        out <- holdout_cv_county_1step_lastsmoothed(
          county_data = county_df,
          window_size = window,
          holdout_frac = holdout_frac,
          min_train = min_train
        )
        if (!is.null(out) && nrow(out) > 0) {
          out <- out %>%
            dplyr::mutate(
              geoid = county_df$geoid[1],
              county = county_df$county[1],
              state = county_df$state[1],
              scenario = scenario_name,
              window_size = window
            )
        }
        out
      })
    
    county_details <- dplyr::bind_rows(county_details[!sapply(county_details, is.null)])
    if (nrow(county_details) == 0) next
    
    # Per-county metrics
    county_summary <- county_details %>%
      dplyr::group_by(geoid, county, state, scenario, window_size) %>%
      dplyr::summarise(
        rmse = sqrt(mean(sq_error, na.rm = TRUE)),
        mae  = mean(abs_error, na.rm = TRUE),
        n_test_days = dplyr::n(),
        .groups = "drop"
      )
    
    # Across-county summary (mean + median/IQR)
    overall_summary <- county_summary %>%
      dplyr::summarise(
        n_counties = dplyr::n(),
        total_test_days = sum(n_test_days),
        
        rmse_mean = mean(rmse, na.rm = TRUE),
        mae_mean  = mean(mae, na.rm = TRUE),
        
        rmse_median = median(rmse, na.rm = TRUE),
        rmse_q1 = quantile(rmse, 0.25, na.rm = TRUE),
        rmse_q3 = quantile(rmse, 0.75, na.rm = TRUE),
        
        mae_median = median(mae, na.rm = TRUE),
        mae_q1 = quantile(mae, 0.25, na.rm = TRUE),
        mae_q3 = quantile(mae, 0.75, na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        scenario = scenario_name,
        window_size = window
      )
    
    results[[as.character(window)]] <- list(
      summary = overall_summary,
      county_summary = county_summary
    )
    
    cat(sprintf("    MAE mean=%.2f | median=%.2f ; RMSE mean=%.2f | median=%.2f\n",
                overall_summary$mae_mean, overall_summary$mae_median,
                overall_summary$rmse_mean, overall_summary$rmse_median))
  }
  
  results
}

# -----------------------------
# Scenarios
# -----------------------------
scenarios <- list(
  scenario1 = list(name = "Scenario-1", start = "2020-06-01", end = "2021-02-02"),
  scenario2 = list(name = "Scenario-2", start = "2022-05-01", end = "2023-03-04")
)

# -----------------------------
# Load data for 2020-2023
# -----------------------------
cat("Loading data for South Carolina counties...\n")
data_list <- lapply(2020:2023, load_and_prepare_data)
full_data <- dplyr::bind_rows(data_list)

n_counties <- full_data %>% dplyr::distinct(geoid, county) %>% nrow()
cat(sprintf("Found %d counties in South Carolina (after removing 'Unknown')\n", n_counties))

# Complete missing dates
full_data <- complete_county_dates(full_data)

# -----------------------------
# Run CV
# -----------------------------
WINDOW_SIZES <- c(3, 5, 7, 10, 14)

all_results <- list()
for (scenario_id in names(scenarios)) {
  sc <- scenarios[[scenario_id]]
  all_results[[scenario_id]] <- evaluate_window_holdout(
    data = full_data,
    scenario_name = sc$name,
    start_date = sc$start,
    end_date = sc$end,
    window_sizes = WINDOW_SIZES,
    holdout_frac = 0.20,
    min_train = 60
  )
}

# -----------------------------
# Compile tables
# -----------------------------
summary_table <- dplyr::bind_rows(
  lapply(names(all_results), function(scenario_id) {
    scenario_results <- all_results[[scenario_id]]
    dplyr::bind_rows(lapply(scenario_results, function(x) x$summary))
  })
)

county_level_table <- dplyr::bind_rows(
  lapply(names(all_results), function(scenario_id) {
    scenario_results <- all_results[[scenario_id]]
    dplyr::bind_rows(lapply(scenario_results, function(x) x$county_summary))
  })
)

# Save tables (MAE + RMSE only)
readr::write_csv(summary_table, "cv_holdout20_1day_summary_MAE_RMSE.csv")
readr::write_csv(county_level_table, "cv_holdout20_1day_by_county_MAE_RMSE.csv")

cat("\n=== SUMMARY (lower is better) ===\n")
print(
  summary_table %>%
    dplyr::select(
      scenario, window_size,
      mae_mean, mae_median, mae_q1, mae_q3,
      rmse_mean, rmse_median, rmse_q1, rmse_q3,
      n_counties
    ) %>%
    dplyr::arrange(scenario, window_size)
)

# -----------------------------
# Publication-ready Supplement table
# -----------------------------
supp_table <- summary_table %>%
  dplyr::mutate(
    scenario = factor(scenario, levels = c("Scenario-1", "Scenario-2")),
    window_size = factor(window_size, levels = WINDOW_SIZES)
  ) %>%
  dplyr::arrange(scenario, window_size) %>%
  dplyr::transmute(
    Scenario = as.character(scenario),
    Window_days = as.integer(as.character(window_size)),
    `MAE mean` = round(mae_mean, 2),
    `MAE median` = round(mae_median, 2),
    `MAE IQR (Q1–Q3)` = sprintf("%.2f–%.2f", mae_q1, mae_q3),
    `RMSE mean` = round(rmse_mean, 2),
    `RMSE median` = round(rmse_median, 2),
    `RMSE IQR (Q1–Q3)` = sprintf("%.2f–%.2f", rmse_q1, rmse_q3),
    `N counties` = n_counties
  )

readr::write_csv(supp_table, "cv_holdout20_1day_supp_table_MAE_RMSE.csv")

# ============================================================
# TWO STACKED PLOTS: MAE (top) + RMSE (bottom)
# Bars = mean; Point = median; Error bars = IQR
# No titles; Caption will describe everything
# ============================================================

plot_df <- summary_table %>%
  dplyr::mutate(
    scenario = factor(scenario, levels = c("Scenario-1", "Scenario-2")),
    window_factor = factor(window_size, levels = WINDOW_SIZES)
  )

plot_metric <- function(df, metric = c("MAE", "RMSE")) {
  
  metric <- match.arg(metric)
  
  if (metric == "MAE") {
    df <- df %>%
      dplyr::mutate(
        mean_value   = mae_mean,
        median_value = mae_median,
        q1_value     = mae_q1,
        q3_value     = mae_q3
      )
    ylab <- "Error magnitude (MAE)"
  } else {
    df <- df %>%
      dplyr::mutate(
        mean_value   = rmse_mean,
        median_value = rmse_median,
        q1_value     = rmse_q1,
        q3_value     = rmse_q3
      )
    ylab <- "Error magnitude (RMSE)"
  }
  
  dodge <- position_dodge(width = 0.80)
  
  ggplot(df, aes(x = window_factor)) +
    geom_col(
      aes(y = mean_value, fill = scenario),
      position = dodge,
      width = 0.72,
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = q1_value, ymax = q3_value, color = scenario),
      position = dodge,
      width = 0.18,
      linewidth = 0.6
    ) +
    geom_point(
      aes(y = median_value, color = scenario),
      position = dodge,
      size = 2.3
    ) +
    labs(
      x = "Smoothing window (days)",
      y = ylab,
      fill = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

p_mae  <- plot_metric(plot_df, "MAE")
p_rmse <- plot_metric(plot_df, "RMSE")

combined_plot <- p_mae / p_rmse +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Helper: save PNG+PDF+SVG
save_all <- function(p, base, w = 12, h = 9, dpi = 300) {
  ggsave(paste0(base, ".png"), p, width = w, height = h, dpi = dpi)
  ggsave(paste0(base, ".pdf"), p, width = w, height = h)
  ggsave(paste0(base, ".svg"), p, width = w, height = h)
}

save_all(combined_plot, "cv_holdout20_1day_MAE_top_RMSE_bottom", w = 12, h = 9)

cat("\n=== OUTPUTS SAVED ===\n")
cat("Tables:\n")
cat("  - cv_holdout20_1day_summary_MAE_RMSE.csv\n")
cat("  - cv_holdout20_1day_by_county_MAE_RMSE.csv\n")
cat("  - cv_holdout20_1day_supp_table_MAE_RMSE.csv\n")
cat("Figure:\n")
cat("  - cv_holdout20_1day_MAE_top_RMSE_bottom.(png/pdf/svg)\n")
cat("\n=== DONE ===\n")
