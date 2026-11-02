# ===================================
# Script Name: ps3_script_Huff.R
# Purpose: Assignment 3 - Time-Series Analysis
# Author: Jarrett Huff
# ===================================


# 2.1 Load Libraries & Import Data ----------------

library(tidyverse)
library(knitr)
library(fpp3) # Brings in tsibble, tsibbledata, and forecasting tools

# Read the manually downloaded FRED CSV using relative path
retail_sales_raw <- read_csv("data/fredgraph.csv")

# Preview the raw wide data
head(retail_sales_raw)


# 2.2 Explicit Column Types and Missing Values -----

# Re-read the file specifying column types and handling FRED missing periods (".")
retail_raw <- read_csv(
  "data/fredgraph.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)


# 2.3 Reshape Data to Long Format -----------------

retail_raw_long <- retail_raw |>  
  select(observation_date, RSAFS, RSAFSNA) |>  
  pivot_longer(
    cols = c(RSAFS, RSAFSNA), 
    names_to = "series_id", 
    values_to = "sales"
  )

# Inspect long format data
head(retail_raw_long)


# 2.4 Build Your tsibble --------------------------

# Convert date to monthly index and construct the tsibble object
retail_ts <- retail_raw_long |>  
  mutate(Month = yearmonth(observation_date)) |>  
  as_tsibble(index = Month, key = series_id)

# Inspect tsibble and check for time gaps
head(retail_ts)
has_gaps(retail_ts)


# 2.5 Canadian Gas Production ---------------------

# Inspect built-in canadian_gas dataset structure
head(canadian_gas)

# Identify key and index variables for canadian_gas
key_vars(canadian_gas)
index_var(canadian_gas)

# Create a working copy (can_gas), select specific columns, and check for gaps
can_gas <- canadian_gas |>  
  select(Month, Volume)

has_gaps(can_gas)

# Inspect the new working tsibble
head(can_gas)
# 2.6 Working with Dates in a tsibble ----------------

# 2.6a Filtering by date (January 1990 onward)
can_gas_recent <- can_gas |>  
  filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)

# 2.6b Extracting date parts and monthly summary
can_gas_recent <- can_gas_recent |>  
  mutate(
    year    = year(Month),         
    month   = month(Month, label = TRUE),         
    quarter = quarter(Month)
  )

# Compute average volume by calendar month (requires converting tsibble to tibble)
can_gas_recent |>  
  as_tibble() |>  
  group_by(month) |>  
  summarize(avg_volume = mean(Volume))

# 2.6c Date arithmetic examples
can_gas_recent |>  
  mutate(
    next_month           = Month + 1,         
    same_month_last_year = Month - 12
  ) |>  
  head()

# 2.6d Changing frequency from monthly to quarterly (dropping incomplete 2005 Q1)
can_gas_q <- can_gas |>  
  filter(Month <= yearmonth("2004 Dec")) |>  
  index_by(Quarter = yearquarter(Month)) |>  
  summarize(Volume = sum(Volume))

head(can_gas_q)
can_gas_q |> autoplot(Volume)


# 2.7 Plotting with autoplot() --------------------

# 2.7a Quick autoplot of U.S. retail sales
retail_ts |> autoplot(sales)

# 2.7b Layered ggplot on U.S. retail sales
retail_ts |>  
  autoplot(sales) +  
  labs(    
    title = "U.S. Retail and Food Services Sales",    
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",    
    x = NULL, 
    y = "Millions of dollars", 
    color = NULL  
  ) +  
  theme_minimal()

# 2.7c Plot Canadian gas production with custom layers
can_gas |>  
  autoplot(Volume) +
  labs(
    title = "Canadian Monthly Natural Gas Production",
    subtitle = "Measured in billions of cubic meters (1960–2005)",
    x = "Month",
    y = "Volume (billion cubic meters)"
  ) +
  theme_minimal()

# 2.7d Custom plot choice (adding a smooth trend line to Canadian gas)
can_gas |>  
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.8) +
  labs(
    title = "Canadian Monthly Natural Gas Production with Trend",
    subtitle = "Featuring a LOESS smooth curve to highlight long-term movement",
    x = "Month",
    y = "Volume"
  ) +
  theme_minimal()

# 3.1 Log transformation -------------------------

# 3.1b Add log_volume to can_gas and plot
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Monthly Natural Gas Production",
    x = "Month",
    y = "Log(Volume)"
  ) +
  theme_minimal()

# 3.1c Log transformation for NSA retail series (RSAFSNA)
retail_nsa_logged <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa_logged |>
  autoplot(log_sales) +
  labs(
    title = "Log of U.S Retail Sales (Not Seasonally Adjusted)",
    x = "Month",
    y = "Log(Sales)"
  ) +
  theme_minimal()

# 3.2 Growth Rates -------------------------

# 3.2a Compute direct growth rate and log differerence approximation
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume),
    growth_gap = abs(growth_direct - growth_logdiff)
  )

# Plot direct growth rate
can_gas |> 
  autoplot(growth_direct) +
  labs( 
   title = " Month over Month Direct Percentage Growth Rate Canadian Gas",
   x = "Month",
   y = "Growth Rate (%)"
  ) +
  theme_minimal()

# Plot the approximation gap
can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Absolute Difference Between Direct Growth and Log-Difference",
    x = "Month",
    y = "Absolute Gap (%)"
  ) +
  theme_minimal()

# 3.3 SA vs. NSA Growth Rates ---------------------

retail_ts <- retail_ts |>  
  group_by_key() |>  
  mutate(growth_mom = 100 * difference(log(sales))) |>  
  ungroup()

retail_ts |>  
  autoplot(growth_mom) +  
  labs(    
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",    
    x = NULL, 
    y = "Growth rate (%)", 
    color = NULL  
  ) +  
  theme_minimal()


# The month over month growth rate of the Not Seasonally Adjusted series is significantly nosier than that of the Seasonally Adjusted series, exhibiting massive, recurring spikes and drops every year. This extreme volatilly in the NSA series is driven by predictable calendar events and holiday shopping cycles, such as massive December retail surges followed by sharp Jan contractions. In contrast, seasonal adjustment filters out this predictable intra year calender noise, leaving the SA growth rate much smoother and better reflective of true underlying economic momentum, consistent with the observation in Part 2.7b#

# ===================================
# Part 4: Seeing, Decomposing, and Modeling Seasonality
# ===================================

# 4.1 Season Plots --------------------------------

# 4.1a Season plot for NSA retail sales
retail_ts |>  
  filter(series_id == "RSAFSNA") |>  
  gg_season(sales) +  
  labs(    
    title = "Seasonal Pattern in NSA Retail Sales",    
    y = "Millions of dollars"  ) +  
  theme_minimal()

# 4.1b Season plot for SA retail sales (RSAFS)
retail_ts |>  
  filter(series_id == "RSAFS") |>  
  gg_season(sales) +  
  labs(    
    title = "Seasonal Pattern in SA Retail Sales",    
    y = "Millions of dollars"  ) +  
  theme_minimal()

# 4.1c Season plot for logged Canadian gas production
can_gas |>  
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Logged Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()


# 4.2 Subseries Plot ------------------------------

can_gas |>  
  gg_subseries(log_volume) +  
  labs(    
    title = "Subseries Plot: Canadian Gas Production",    
    y = "log(Volume)"  ) +
  theme_minimal()


# 4.3 Seasonally Adjusting with X-13ARIMA-SEATS ---

# 4.3a Run X-13 on NSA retail series and plot components
retail_nsa_x13 <- retail_ts |>  
  filter(series_id == "RSAFSNA") |>  
  model(x13 = X_13ARIMA_SEATS(sales)) |>  
  components()

retail_nsa_x13 |> autoplot()

# 4.3b Compare custom X-13 adjustment to the official Census Bureau series
sa_compare <- retail_nsa_x13 |>  
  as_tibble() |>  
  select(Month, mine = season_adjust) |>  
  left_join(    
    retail_ts |>      
      filter(series_id == "RSAFS") |>      
      as_tibble() |>      
      transmute(Month, census = sales),    
    by = "Month"  ) |>  
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>  
  ggplot(aes(x = Month, y = pct_diff)) +  
  geom_line() +  
  labs(    
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",    
    x = NULL, 
    y = "Difference (%)"  ) +  
  theme_minimal()


# 4.4 Simple Model: Linear Trend + Seasonal Dummies ---

# 4.4a Fit TSLM model to log_volume
can_gas_fit <- can_gas |>  
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

# Produce formatted coefficient table for write-up
can_gas_fit |>  
  tidy() |>  
  select(term, estimate, std.error, statistic, p.value) |>  
  knitr::kable(digits = 3)

# 4.4b Plot fitted values against actual data
can_gas_fit |>  
  augment() |>  
  ggplot(aes(x = Month)) +  
  geom_line(aes(y = log_volume, color = "Actual")) +  
  geom_line(aes(y = .fitted, color = "Fitted")) +  
  labs(
    title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",       
    y = "log(Volume)", 
    color = NULL
  ) +  
  theme_minimal()

# 4.4c Residual Diagnostics & Ljung-Box Test
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>  
  augment() |>  
  features(.innov, ljung_box, lag = 24)

# ===================================
# Part 5: A First Forecast Evaluation
# ===================================

# 5.1 Expanding-Window Time-Series Cross-Validation ---

# Run expanding-window TS-CV (init = 240 months) and evaluate accuracy
gas_cv_fc <- can_gas |>  
  select(Month, Volume) |>  
  stretch_tsibble(.init = 240, .step = 1) |>  
  model(
    snaive = SNAIVE(Volume),        
    tslm   = TSLM(log(Volume) ~ trend() + season())
  ) |>  
  forecast(h = 1)

gas_cv_fc |>  
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))


# 5.2 Diebold-Mariano Test for Forecast Accuracy ---

# Align forecast errors and run Diebold-Mariano test (absolute loss, power = 1)
errs <- gas_cv_fc |>  
  as_tibble() |>  
  left_join(
    as_tibble(can_gas) |> select(Month, actual = Volume),            
    by = "Month"
  ) |>  
  mutate(e = actual - .mean) |>  
  select(Month, .model, e) |>  
  pivot_wider(names_from = .model, values_from = e) |>  
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)


# 5.3 Rolling Window Time-Series Cross-Validation ---

# Run rolling-window TS-CV (size = 240 months) and evaluate accuracy
gas_rolling_fc <- can_gas |>  
  select(Month, Volume) |>  
  slide_tsibble(.size = 240, .step = 1) |>  
  model(
    snaive = SNAIVE(Volume),        
    tslm   = TSLM(log(Volume) ~ trend() + season())
  ) |>  
  forecast(h = 1)

gas_rolling_fc |>  
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))