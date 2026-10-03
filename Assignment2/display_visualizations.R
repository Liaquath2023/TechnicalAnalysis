# BDA400 Assignment 2
# Data display and visualizations
# Student: M A Salaam Siddiqui

library(quantmod)
library(TTR)
library(ggplot2)
library(dplyr)

source("technical_analysis_functions.R")

# Load portfolio data
portfolio_data <- load_stock_data("portfolio.txt", from = Sys.Date() - 365)

# Display imported data
for (symbol in names(portfolio_data)) {
  cat("\n==============================\n")
  cat("Imported data:", symbol, "\n")
  cat("==============================\n")
  print(head(portfolio_data[[symbol]], 10))
}

# Calculate and display required statistics
statistics <- calculate_portfolio_statistics(portfolio_data)
print(statistics)

# Create a combined data frame for plotting
plot_data <- bind_rows(lapply(names(portfolio_data), function(symbol) {
  df <- portfolio_data[[symbol]]
  data.frame(
    Date = as.Date(index(df)),
    Close = as.numeric(Cl(df)),
    Symbol = symbol
  )
}))

# Line chart of closing prices
p1 <- ggplot(plot_data, aes(x = Date, y = Close)) +
  geom_line() +
  facet_wrap(~ Symbol, scales = "free_y") +
  labs(
    title = "Portfolio Closing Prices – Last 12 Months",
    x = "Date",
    y = "Closing Price"
  ) +
  theme_minimal()

print(p1)
ggsave("portfolio_closing_prices.png", p1, width = 10, height = 7, dpi = 300)

# Bar chart of mean and median
summary_long <- statistics %>%
  select(Symbol, Mean, Median) %>%
  tidyr::pivot_longer(cols = c(Mean, Median),
                      names_to = "Statistic",
                      values_to = "Value")

p2 <- ggplot(summary_long, aes(x = Symbol, y = Value, fill = Statistic)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean vs. Median Closing Price",
    x = "Stock Symbol",
    y = "Price"
  ) +
  theme_minimal()

print(p2)
ggsave("mean_median_comparison.png", p2, width = 10, height = 7, dpi = 300)

# 20-day moving average chart for each stock
ma_plot_data <- bind_rows(lapply(names(portfolio_data), function(symbol) {
  df <- portfolio_data[[symbol]]
  close <- as.numeric(Cl(df))
  data.frame(
    Date = as.Date(index(df)),
    Close = close,
    MovingAverage20 = as.numeric(SMA(close, n = 20)),
    Symbol = symbol
  )
}))

p3 <- ggplot(ma_plot_data, aes(x = Date)) +
  geom_line(aes(y = Close), linewidth = 0.5) +
  geom_line(aes(y = MovingAverage20), linewidth = 0.8) +
  facet_wrap(~ Symbol, scales = "free_y") +
  labs(
    title = "Closing Price and 20-Day Moving Average",
    x = "Date",
    y = "Price"
  ) +
  theme_minimal()

print(p3)
ggsave("moving_average_20_day.png", p3, width = 10, height = 7, dpi = 300)

# Optional console confirmation
cat("\nAnalysis complete. Output files created:\n")
cat("- portfolio_closing_prices.png\n")
cat("- mean_median_comparison.png\n")
cat("- moving_average_20_day.png\n")
