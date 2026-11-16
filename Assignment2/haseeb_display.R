# haseeb_display.R
# BDA400 Assignment 2 | Haseeb
# Utilities to display the imported stock data in the styles used by finance
# sites like Yahoo Finance: a quote-style snapshot, a recent-history table,
# and a price chart with the 20-day moving average overlaid.

source("haseeb_load_and_stats.R")

# Yahoo-style snapshot: last close, day change, 52-period high/low, volume.
display_snapshot <- function(sym, stock_data) {
  closes <- as.numeric(Cl(stock_data))
  n <- length(closes)
  chg <- closes[n] - closes[n - 1]
  cat(sprintf("%-6s last: %8.2f | chg: %+7.2f (%+5.2f%%) | hi: %8.2f | lo: %7.2f | vol: %s\n",
              sym, closes[n], chg, 100 * chg / closes[n - 1],
              max(Hi(stock_data)), min(Lo(stock_data)),
              format(as.numeric(tail(Vo(stock_data), 1)), big.mark = ",")))
}

# Recent-history table, newest first, like the Historical Data tab on Yahoo.
display_history <- function(sym, stock_data, days = 5) {
  cat("\n---", sym, "last", days, "sessions ---\n")
  print(tail(stock_data, days))
}

# Price chart with the moving average overlay, saved as a PNG.
display_chart <- function(sym, stock_data, ma_period = 20) {
  png(paste0("chart_", sym, ".png"), width = 900, height = 500)
  closes <- Cl(stock_data)
  plot(index(stock_data), as.numeric(closes), type = "l", lwd = 2,
       col = "steelblue", main = paste(sym, "close with", ma_period, "day SMA"),
       xlab = "Date", ylab = "Close (USD)")
  lines(index(stock_data), as.numeric(TTR::SMA(closes, ma_period)),
        col = "tomato", lwd = 2, lty = 2)
  legend("topleft", c("Close", paste0("SMA", ma_period)),
         col = c("steelblue", "tomato"), lwd = 2, lty = c(1, 2), bty = "n")
  dev.off()
}

# ---- Run everything ----
stocks <- load_stock_data("portfolio.txt")
cat("\n================ PORTFOLIO SNAPSHOT ================\n")
for (sym in names(stocks)) display_snapshot(sym, stocks[[sym]])
cat("\n================ STATISTICS ================\n")
print(portfolio_statistics(stocks), digits = 6)
for (sym in names(stocks)) display_history(sym, stocks[[sym]])
for (sym in names(stocks)) display_chart(sym, stocks[[sym]])
cat("\nCharts written:", paste(paste0("chart_", names(stocks), ".png"), collapse = ", "), "\n")
