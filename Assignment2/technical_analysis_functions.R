# BDA400 Assignment 2
# Technical Analysis using R - Preliminary Stage
# Student: M A Salaam Siddiqui

# Install packages once if they are not already installed:
# install.packages(c("quantmod", "TTR", "ggplot2", "dplyr"))

library(quantmod)
library(TTR)

# ------------------------------------------------------------
# Function 1: Load stock data
# ------------------------------------------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt",
                             from = Sys.Date() - 365) {
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]

  if (length(symbols) == 0) {
    stop("portfolio.txt does not contain any stock symbols.")
  }

  stock_data <- list()

  for (symbol in symbols) {
    message("Downloading data for ", symbol, " ...")
    x <- tryCatch(
      getSymbols(Symbols = symbol,
                 src = "yahoo",
                 from = from,
                 auto.assign = FALSE,
                 warnings = FALSE),
      error = function(e) {
        warning("Could not load ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(x)) {
      stock_data[[symbol]] <- x
    }
  }

  if (length(stock_data) == 0) {
    stop("No stock data could be downloaded. Check your Internet connection and symbols.")
  }

  return(stock_data)
}

# ------------------------------------------------------------
# Helper: statistical mode
# ------------------------------------------------------------
calculate_mode <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)

  counts <- table(x)
  modes <- as.numeric(names(counts)[counts == max(counts)])

  # For continuous stock prices, every exact value may occur once.
  # In that case, report NA rather than claiming a meaningful mode.
  if (length(modes) == length(unique(x)) && max(counts) == 1) {
    return(NA_real_)
  }

  return(modes[1])
}

# ------------------------------------------------------------
# Function 2: Calculate required statistics
# ------------------------------------------------------------
calculate_statistics <- function(stock_df, ma_n = 20) {
  close_prices <- as.numeric(Cl(stock_df))
  close_prices <- close_prices[is.finite(close_prices)]

  if (length(close_prices) == 0) {
    stop("No valid closing prices were found.")
  }

  ma_values <- SMA(close_prices, n = ma_n)
  latest_ma <- tail(na.omit(ma_values), 1)

  stats <- data.frame(
    Observations = length(close_prices),
    Moving_Average_20_Day = as.numeric(latest_ma),
    Mean = mean(close_prices),
    Mode = calculate_mode(round(close_prices, 2)),
    Median = median(close_prices),
    Standard_Deviation = sd(close_prices)
  )

  return(stats)
}

# ------------------------------------------------------------
# Function 3: Calculate statistics for every stock
# ------------------------------------------------------------
calculate_portfolio_statistics <- function(stock_data, ma_n = 20) {
  results <- lapply(stock_data, calculate_statistics, ma_n = ma_n)
  results_df <- do.call(rbind, results)
  results_df$Symbol <- rownames(results_df)
  rownames(results_df) <- NULL
  results_df <- results_df[, c("Symbol", setdiff(names(results_df), "Symbol"))]
  return(results_df)
}

# ------------------------------------------------------------
# Function 4: Display a stock data frame
# ------------------------------------------------------------
display_stock_data <- function(stock_data, symbol, n = 10) {
  if (!symbol %in% names(stock_data)) {
    stop("Symbol not found in loaded stock data.")
  }

  print(head(stock_data[[symbol]], n))
}

# ------------------------------------------------------------
# Example execution
# ------------------------------------------------------------
portfolio_data <- load_stock_data("portfolio.txt", from = Sys.Date() - 365)
statistics <- calculate_portfolio_statistics(portfolio_data)

print(statistics)

# Display first 10 rows for every loaded stock.
for (symbol in names(portfolio_data)) {
  cat("\n==============================\n")
  cat("Stock:", symbol, "\n")
  cat("==============================\n")
  display_stock_data(portfolio_data, symbol, n = 10)
}
