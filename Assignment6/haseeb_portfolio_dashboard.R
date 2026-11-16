# haseeb_portfolio_dashboard.R
# BDA400 Assignment 6 | Technical Analysis using R, Visualization Phase
# Haseeb
#
# A portfolio dashboard built with shiny, ggplot2 and quantmod. It fetches
# stock data from Yahoo Finance, draws the price as a line, candlestick or
# area chart, overlays moving averages with on/off toggles, shows RSI and
# MACD panels, and annotates the chart with Buy / Sell signals from a
# 20 over 50 day moving average crossover rule.
#
# How to run:   shiny::runApp("haseeb_portfolio_dashboard.R")
# Packages:     install.packages(c("shiny", "ggplot2", "quantmod"))
#               (quantmod brings TTR along for the indicators)

# ==== Step 1: Data Collection and Setup ====================================
library(shiny)
library(ggplot2)
library(quantmod)

# ---- helper: xts stock data to a plain data frame for ggplot ----
# we convert because ggplot2 is much easier to layer with a data frame
ohlc_to_df <- function(x) {
  data.frame(
    Date = index(x),
    Open = as.numeric(Op(x)),
    High = as.numeric(Hi(x)),
    Low = as.numeric(Lo(x)),
    Close = as.numeric(Cl(x))
  )
}

# ---- helper: trading rule ----
# Moving Average Crossover Trading Rule
# If the short-term MA crosses above the long-term MA, generate a 'Buy'.
# If the short-term MA crosses below the long-term MA, generate a 'Sell'.
# Otherwise the signal for that bar is 'Hold'.
compute_signals <- function(df, short_n = 20, long_n = 50) {
  # on a short window (e.g. a monthly view) there may be fewer bars than
  # the MA length; SMA() would stop with an error, so we fall back to NA
  # and the rule simply produces no signals for that view
  ma_or_na <- function(x, n) {
    if (n >= length(x)) rep(NA_real_, length(x)) else SMA(x, n = n)
  }
  df$short_ma <- ma_or_na(df$Close, short_n)
  df$long_ma <- ma_or_na(df$Close, long_n)
  state <- ifelse(df$short_ma > df$long_ma, "Buy", "Sell")
  # a bar only gets a Buy / Sell label on the bar where the cross happens,
  # every other bar is a Hold, so the chart stays readable
  df$Signal <- "Hold"
  for (i in 2:nrow(df)) {
    if (!is.na(state[i]) && !is.na(state[i - 1]) && state[i] != state[i - 1]) {
      df$Signal[i] <- state[i]
    }
  }
  df
}

# ==== Step 2: UI with interactive widgets ==================================
ui <- fluidPage(
  titlePanel("Portfolio Dashboard - Haseeb (BDA400 A6)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      radioButtons("chart_type", "Chart Type:",
                   choices = c("Line", "Candlestick", "Area")),
      checkboxGroupInput("technical_indicators", "Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      checkboxInput("show_signals", "Show Buy / Sell annotations", TRUE),
      actionButton("load", "Load Data", class = "btn-primary")
    ),
    mainPanel(
      width = 9,
      textOutput("status"),
      plotOutput("stock_chart", height = "420px"),
      # the oscillator panels only appear when their indicator is ticked
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') > -1",
        plotOutput("rsi_chart", height = "160px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') > -1",
        plotOutput("macd_chart", height = "160px")
      )
    )
  )
)

# ==== Steps 2 to 4: server logic ===========================================
server <- function(input, output, session) {

  # fetch from Yahoo Finance only when the button is pressed, with error
  # handling so a bad ticker shows a message instead of crashing the app
  stock_data <- eventReactive(input$load, {
    tryCatch(
      getSymbols(toupper(trimws(input$symbol)), src = "yahoo",
                 from = input$date_range[1], to = input$date_range[2],
                 auto.assign = FALSE),
      error = function(e) NULL
    )
  }, ignoreNULL = FALSE)

  # aggregate the daily bars when Weekly or Monthly is selected
  filtered_data <- reactive({
    raw <- stock_data()
    if (is.null(raw)) return(NULL)
    if (input$time_frame == "Weekly") raw <- to.weekly(raw)
    if (input$time_frame == "Monthly") raw <- to.monthly(raw)
    df <- ohlc_to_df(raw)
    compute_signals(df)
  })

  output$status <- renderText({
    if (input$load == 0) {
      "Press Load Data to fetch the selected symbol from Yahoo Finance."
    } else if (is.null(stock_data())) {
      "Could not fetch that symbol. Check the ticker and try again."
    } else {
      paste0(toupper(trimws(input$symbol)), ": ",
             nrow(filtered_data()), " bars loaded (", input$time_frame, ").")
    }
  })

  output$stock_chart <- renderPlot({
    df <- filtered_data()
    validate(need(!is.null(df), "No data loaded yet."))

    p <- ggplot(df, aes(x = Date))

    # ---- chart types ----
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), color = "#14396B", linewidth = 0.6)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "#14396B", alpha = 0.25) +
        geom_line(aes(y = Close), color = "#14396B", linewidth = 0.5)
    } else {
      # candlestick drawn from scratch with ggplot2: a thin segment for the
      # high-low range and a thicker segment for the open-close body
      up <- df$Close >= df$Open
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High),
                     color = "grey40", linewidth = 0.3) +
        geom_segment(aes(xend = Date, y = Open, yend = Close),
                     color = ifelse(up, "#1A9850", "#D73027"),
                     linewidth = 2)
    }

    # ---- indicator overlay: moving averages on the price chart ----
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = short_ma, color = "SMA 20"), linewidth = 0.6, na.rm = TRUE) +
        geom_line(aes(y = long_ma, color = "SMA 50"), linewidth = 0.6, na.rm = TRUE) +
        scale_color_manual(name = "Overlay",
                           values = c("SMA 20" = "#E69F00", "SMA 50" = "#9467BD"))
    }

    # ---- trading rule annotations on the bars ----
    if (isTRUE(input$show_signals)) {
      marks <- df[df$Signal != "Hold", ]
      if (nrow(marks) > 0) {
        p <- p +
          geom_point(data = marks,
                     aes(y = Close),
                     shape = ifelse(marks$Signal == "Buy", 24, 25),
                     fill = ifelse(marks$Signal == "Buy", "#1A9850", "#D73027"),
                     size = 3) +
          geom_text(data = marks,
                    aes(y = Close, label = Signal),
                    vjust = ifelse(marks$Signal == "Buy", 1.8, -1.0),
                    size = 3, fontface = "bold",
                    color = ifelse(marks$Signal == "Buy", "#1A9850", "#D73027"))
      }
    }

    p + labs(title = paste(toupper(trimws(input$symbol)), "-", input$chart_type,
                           "chart with 20/50 SMA crossover signals"),
             x = "Date", y = "Price (USD)") +
      theme_minimal(base_size = 13)
  })

  # ---- RSI panel ----
  output$rsi_chart <- renderPlot({
    df <- filtered_data()
    validate(need(!is.null(df) && nrow(df) > 15, "Not enough data for RSI."))
    df$rsi <- RSI(df$Close, n = 14)
    ggplot(df, aes(Date, rsi)) +
      geom_line(color = "#14396B", na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "grey50") +
      labs(title = "RSI (14)", x = NULL, y = "RSI") +
      ylim(0, 100) +
      theme_minimal(base_size = 12)
  })

  # ---- MACD panel ----
  output$macd_chart <- renderPlot({
    df <- filtered_data()
    validate(need(!is.null(df) && nrow(df) > 35, "Not enough data for MACD."))
    m <- as.data.frame(MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9))
    df$macd <- m$macd
    df$macd_signal <- m$signal
    ggplot(df, aes(Date)) +
      geom_line(aes(y = macd), color = "#14396B", na.rm = TRUE) +
      geom_line(aes(y = macd_signal), color = "#E69F00", na.rm = TRUE) +
      geom_hline(yintercept = 0, color = "grey50") +
      labs(title = "MACD (12, 26, 9) with signal line", x = NULL, y = "MACD") +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)
