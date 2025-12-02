# ============================================================================
# Commodity Prices Comparison - Server Logic
# ============================================================================

commodity_server <- function(input, output, session, macro_data, shared_state) {
  
  # This analysis is limited to United States only
  selected_country <- "United States"
  
  # Animation state
  is_playing <- reactiveVal(FALSE)
  
  # Cache for commodity data (gold and oil)
  gold_cache <- reactiveVal(NULL)
  oil_cache <- reactiveVal(NULL)
  
  # Get year range from data
  year_range <- reactive({
    req(macro_data)
    years <- sort(unique(macro_data$year))
    list(min = min(years, na.rm = TRUE), max = max(years, na.rm = TRUE), all = years)
  })
  
  # Initialize year slider
  observe({
    years <- year_range()$all
    updateSliderInput(session, "commodity_year", min = min(years), max = max(years), 
                     value = max(years, na.rm = TRUE))
  })
  
  # Play/Pause button handler
  observeEvent(input$commodity_play_pause, {
    if (is_playing()) {
      # Pause animation
      is_playing(FALSE)
      updateActionButton(session, "commodity_play_pause", label = "Play", icon = shiny::icon("play"))
    } else {
      # Start animation
      is_playing(TRUE)
      updateActionButton(session, "commodity_play_pause", label = "Pause", icon = shiny::icon("pause"))
    }
  })
  
  # Animation timer - updates every 500ms
  auto_invalidate <- reactiveTimer(500, session)
  
  # Animation logic - triggered by timer when playing
  observe({
    # Trigger timer (this makes the observe block re-run every 500ms)
    auto_invalidate()
    
    # Only proceed if playing
    if (!is_playing()) return()
    
    years <- year_range()$all
    current_year <- input$commodity_year
    current_idx <- which(years == current_year)
    
    if (length(current_idx) == 0) {
      current_idx <- 1
    }
    
    # Move to next year
    if (current_idx < length(years)) {
      next_year <- years[current_idx + 1]
      updateSliderInput(session, "commodity_year", value = next_year)
    } else {
      # Reached the end, stop animation
      is_playing(FALSE)
      updateActionButton(session, "commodity_play_pause", label = "Play", icon = shiny::icon("play"))
    }
  })
  
  # Animation status text
  output$commodity_animation_status <- renderText({
    if (is_playing()) {
      years <- year_range()$all
      current_idx <- which(years == input$commodity_year)
      total <- length(years)
      paste("Playing:", current_idx, "of", total, "years")
    } else {
      ""
    }
  })
  
  # Get Gold Prices data from DataHub.io
  gold_data <- reactive({
    # Return cached data if available
    if (!is.null(gold_cache())) {
      return(gold_cache())
    }
    
    # Fetch gold prices from DataHub.io
    # URL: https://datahub.io/core/gold-prices/r/monthly.csv
    gold_url <- "https://datahub.io/core/gold-prices/r/monthly.csv"
    
    gold_raw <- tryCatch({
      read_csv(gold_url, show_col_types = FALSE)
    }, error = function(e) {
      cat("Warning: Could not fetch gold prices. Error:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(gold_raw) || nrow(gold_raw) == 0) {
      return(data.frame(year = integer(0), price = numeric(0)))
    }
    
    # Debug: Print column names
    cat("Gold data columns:", paste(names(gold_raw), collapse = ", "), "\n")
    if (nrow(gold_raw) > 0) {
      cat("First few rows:\n")
      print(head(gold_raw, 3))
    }
    
    # Process gold data
    # DataHub.io format: Date, Price (USD per troy ounce)
    # Handle different possible column names
    date_col <- if("Date" %in% names(gold_raw)) "Date" 
                else if("date" %in% names(gold_raw)) "date"
                else names(gold_raw)[1]  # Use first column as fallback
    
    price_col <- if("Price" %in% names(gold_raw)) "Price"
                 else if("price" %in% names(gold_raw)) "price"
                 else if("USD (AM)" %in% names(gold_raw)) "USD (AM)"
                 else if(length(names(gold_raw)) >= 2) names(gold_raw)[2]
                 else names(gold_raw)[1]  # Use second column as fallback
    
    cat("Using date column:", date_col, "price column:", price_col, "\n")
    
    # Parse dates row by row with comprehensive error handling
    gold_processed <- tryCatch({
      # Extract date and price columns
      date_vec <- gold_raw[[date_col]]
      price_vec <- gold_raw[[price_col]]
      
      # Parse dates safely
      # DataHub.io gold prices use "YYYY-MM" format (year-month, no day)
      parsed_dates <- rep(as.Date(NA), length(date_vec))
      for (i in seq_along(date_vec)) {
        date_str <- as.character(date_vec[i])
        if (is.na(date_str) || date_str == "" || date_str == "NA") next
        
        # Try formats, prioritizing YYYY-MM format first (most common for this data)
        formats <- c("%Y-%m", "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S", "%d-%m-%Y", "%B %d, %Y", "%d %B %Y")
        parsed <- NA
        for (fmt in formats) {
          try_date <- tryCatch({
            if (fmt == "%Y-%m") {
              # For YYYY-MM format, add "-01" to make it a valid date (first day of month)
              date_with_day <- paste0(date_str, "-01")
              as.Date(date_with_day, format = "%Y-%m-%d")
            } else {
              as.Date(date_str, format = fmt)
            }
          }, error = function(e) NA)
          if (!is.na(try_date)) {
            parsed <- try_date
            break
          }
        }
        
        # If still NA, try automatic parsing
        if (is.na(parsed)) {
          # Try adding "-01" for YYYY-MM format
          if (grepl("^\\d{4}-\\d{2}$", date_str)) {
            parsed <- tryCatch(as.Date(paste0(date_str, "-01")), error = function(e) NA)
          } else {
            parsed <- tryCatch(as.Date(date_str), error = function(e) NA)
          }
        }
        
        parsed_dates[i] <- parsed
      }
      
      # Create result data frame
      result <- data.frame(
        date = parsed_dates,
        price = as.numeric(price_vec),
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          year = as.numeric(format(date, "%Y"))
        ) %>%
        filter(!is.na(date), !is.na(year), !is.na(price), price > 0) %>%
        select(year, price) %>%
        group_by(year) %>%
        summarise(price = mean(price, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(price), !is.na(year))
      
      result
    }, error = function(e) {
      cat("Error processing gold data:", e$message, "\n")
      return(data.frame(year = integer(0), price = numeric(0)))
    })
    
    # Cache the result
    gold_cache(gold_processed)
    
    return(gold_processed)
  })
  
  # Get Oil Prices data (WTI Crude Oil)
  oil_data <- reactive({
    # Return cached data if available
    if (!is.null(oil_cache())) {
      return(oil_cache())
    }
    
    # Fetch WTI crude oil prices from FRED Economic Data
    # Alternative: Use a public CSV source
    # URL format: https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILWTICO
    oil_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILWTICO"
    
    oil_raw <- tryCatch({
      # FRED CSV has DATE and DCOILWTICO columns
      read_csv(oil_url, show_col_types = FALSE, skip = 1, 
               col_names = c("date", "price"))
    }, error = function(e) {
      cat("Warning: Could not fetch oil prices. Error:", e$message, "\n")
      # Fallback: Try alternative source
      return(NULL)
    })
    
    if (is.null(oil_raw) || nrow(oil_raw) == 0) {
      return(data.frame(year = integer(0), price = numeric(0)))
    }
    
    # Process oil data
    oil_processed <- oil_raw %>%
      mutate(
        date = as.Date(date),
        year = as.numeric(format(date, "%Y")),
        price = as.numeric(price)
      ) %>%
      filter(!is.na(year), !is.na(price), price > 0) %>%
      select(year, price) %>%
      group_by(year) %>%
      summarise(price = mean(price, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(price), !is.na(year))
    
    # Cache the result
    oil_cache(oil_processed)
    
    return(oil_processed)
  })
  
  # Get selected commodity data
  commodity_data <- reactive({
    req(input$commodity_type)
    
    if (input$commodity_type == "gold") {
      return(gold_data())
    } else {
      return(oil_data())
    }
  })
  
  # Reactive data - Merge indicator data with commodity prices
  # For animation, we'll show data up to the selected year
  commodity_merged_data <- reactive({
    req(input$commodity_indicator, input$commodity_year, input$commodity_type)
    
    # Get indicator data for United States only, up to selected year
    indicator_data <- macro_data %>%
      filter(
        country == selected_country,
        year <= input$commodity_year,
        !is.na(.data[[input$commodity_indicator]])
      ) %>%
      select(year, indicator_value = .data[[input$commodity_indicator]]) %>%
      arrange(year)
    
    # Get commodity data
    commodity <- commodity_data()
    
    if (nrow(commodity) == 0) {
      return(data.frame(year = integer(0), indicator_value = numeric(0), price = numeric(0)))
    }
    
    # Filter commodity data up to selected year
    commodity <- commodity %>%
      filter(year <= input$commodity_year)
    
    # Merge on year
    merged <- indicator_data %>%
      inner_join(commodity, by = "year") %>%
      filter(!is.na(indicator_value), !is.na(price)) %>%
      arrange(year)
    
    merged
  })
  
  # Plot
  output$commodity_plot <- renderPlotly({
    data <- commodity_merged_data()
    
    validate(
      need(nrow(data) > 0, paste("No data available for selected indicator and year.", 
                                  "Commodity data may not be available for this year."))
    )
    
    # Indicator and commodity labels
    indicator_label <- tools::toTitleCase(gsub("_", " ", input$commodity_indicator))
    commodity_label <- if(input$commodity_type == "gold") "Gold Price" else "Oil Price (WTI)"
    commodity_unit <- if(input$commodity_type == "gold") "USD/oz" else "USD/barrel"
    
    # Create Dual-Axis Plot
    p <- plot_ly(data, x = ~year)
    
    # Line 1 (Left Axis) - Economic Indicator - Royal Blue
    p <- p %>% add_lines(
      y = ~indicator_value, 
      name = indicator_label,
      line = list(color = "#2563eb", width = 3)
    )
    
    # Line 2 (Right Axis) - Commodity Price - Amber/Gold for gold, Dark Green for oil
    commodity_color <- if(input$commodity_type == "gold") "#f59e0b" else "#059669"
    p <- p %>% add_lines(
      y = ~price, 
      name = commodity_label, 
      yaxis = "y2",
      line = list(color = commodity_color, width = 3, dash = "dot")
    )
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = paste("United States:", indicator_label, "vs", commodity_label),
        font = list(color = "#1e293b", size = 18)
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      legend = list(orientation = "h", x = 0.1, y = -0.15),
      
      # Left Y-Axis - Economic Indicator
      yaxis = list(
        title = indicator_label,
        titlefont = list(color = "#2563eb"),
        tickfont = list(color = "#2563eb"),
        showgrid = TRUE,
        gridcolor = "#e2e8f0"
      ),
      
      # Right Y-Axis - Commodity Price
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = paste(commodity_label, "(", commodity_unit, ")"),
        titlefont = list(color = commodity_color),
        tickfont = list(color = commodity_color),
        showgrid = FALSE
      ),
      
      xaxis = list(
        title = "Year",
        showgrid = FALSE
      ),
      
      margin = list(r = 50)
    )
    
    p
  })
  
  # Correlation analysis
  output$commodity_correlation <- renderUI({
    data <- commodity_merged_data()
    
    if (is.null(data) || nrow(data) < 3) {
      return(HTML('<div class="alert alert-warning">Insufficient data points for correlation analysis. Need at least 3 overlapping years.</div>'))
    }
    
    # Calculate correlation
    corr <- tryCatch({
      cor(data$indicator_value, data$price, use = "complete.obs")
    }, error = function(e) {
      cat("Error calculating correlation:", e$message, "\n")
      return(NA)
    })
    
    if (is.na(corr)) {
      return(HTML('<div class="alert alert-warning">Could not calculate correlation coefficient.</div>'))
    }
    
    corr_rounded <- round(corr, 3)
    
    # Interpretation
    if (abs(corr) < 0.3) {
      strength <- "weak"
      color_class <- "secondary"
    } else if (abs(corr) < 0.7) {
      strength <- "moderate"
      color_class <- "info"
    } else {
      strength <- "strong"
      color_class <- if(corr > 0) "success" else "warning"
    }
    
    direction <- if(corr > 0) "positive" else "negative"
    
    indicator_label <- tools::toTitleCase(gsub("_", " ", input$commodity_indicator))
    commodity_label <- if(input$commodity_type == "gold") "gold prices" else "oil prices"
    
    HTML(paste0(
      '<div class="alert alert-', color_class, '">',
      '<h5>Correlation Coefficient: <b>', corr_rounded, '</b></h5>',
      '<p style="margin-bottom: 5px;">This indicates a <b>', strength, '</b> <b>', direction, '</b> correlation between ',
      '<b>', indicator_label, '</b> and <b>', commodity_label, '</b>.</p>',
      '<p style="margin-bottom: 0; font-size: 12px;">Correlation ranges from -1 (perfect negative) to +1 (perfect positive). ',
      'A value close to 0 indicates little to no linear relationship.</p>',
      '</div>'
    ))
  })
}

