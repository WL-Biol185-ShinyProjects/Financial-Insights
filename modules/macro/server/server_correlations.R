# ============================================================================
# Correlations - Server Logic
# ============================================================================

correlations_server <- function(input, output, session, macro_data, shared_state) {
  
  # Animation state
  is_playing <- reactiveVal(FALSE)
  
  # Get year range from data
  year_range <- reactive({
    req(macro_data)
    years <- sort(unique(macro_data$year))
    list(min = min(years, na.rm = TRUE), max = max(years, na.rm = TRUE), all = years)
  })
  
  # Initialize country selector and year slider
  observe({
    countries <- unique(macro_data$country) %>% sort()
    updateSelectizeInput(session, "corr_countries", choices = countries, server = TRUE)
    
    # Initialize year slider with available years
    years <- year_range()$all
    updateSliderInput(session, "corr_year", min = min(years), max = max(years), 
                     value = max(years, na.rm = TRUE))
  })
  
  # Play/Pause button handler
  observeEvent(input$corr_play_pause, {
    if (is_playing()) {
      # Pause animation
      is_playing(FALSE)
      updateActionButton(session, "corr_play_pause", label = "Play", icon = shiny::icon("play"))
    } else {
      # Start animation
      is_playing(TRUE)
      updateActionButton(session, "corr_play_pause", label = "Pause", icon = shiny::icon("pause"))
      # Trigger the animation observe block immediately
      invalidateLater(0, session)
    }
  })
  
  # Animation speed (milliseconds between updates)
  animation_speed <- reactive({
    as.numeric(input$corr_speed)
  })
  
  # Animation logic - triggered by timer when playing
  observe({
    # Only proceed if playing
    if (!is_playing()) return()
    
    years <- year_range()$all
    current_year <- input$corr_year
    current_idx <- which(years == current_year)
    
    if (length(current_idx) == 0) {
      current_idx <- 1
    }
    
    # Move to next year
    if (current_idx < length(years)) {
      next_year <- years[current_idx + 1]
      updateSliderInput(session, "corr_year", value = next_year)
    } else {
      # Reached the end, stop animation
      is_playing(FALSE)
      updateActionButton(session, "corr_play_pause", label = "Play", icon = shiny::icon("play"))
    }
    
    # Re-schedule for next iteration at the specified speed
    invalidateLater(animation_speed(), session)
  })
  
  # Animation status text
  output$corr_animation_status <- renderText({
    if (is_playing()) {
      years <- year_range()$all
      current_idx <- which(years == input$corr_year)
      total <- length(years)
      paste("Playing:", current_idx, "of", total, "years")
    } else {
      ""
    }
  })
  
  # Reactive Correlation Matrix - Auto-updates on input change
  corr_data <- reactive({
    req(input$corr_year)
    
    # Filter data by single year
    data <- macro_data %>%
      filter(year == input$corr_year)
    
    # Filter by country if selected
    if (!is.null(input$corr_countries) && length(input$corr_countries) > 0) {
      data <- data %>% filter(country %in% input$corr_countries)
    }
    
    # Select numeric indicators
    numeric_cols <- c("gdp_per_capita", "inflation", "unemployment", "life_expectancy", "pop_growth")
    
    # Use pairwise complete observations to handle NAs robustly
    # This calculates correlations based on available matching pairs for each variable set
    cor_matrix <- cor(data[, numeric_cols], use = "pairwise.complete.obs")
    
    # Rename columns for display
    colnames(cor_matrix) <- c("GDP/Cap", "Inflation", "Unemployment", "Life Exp", "Pop Growth")
    rownames(cor_matrix) <- colnames(cor_matrix)
    
    cor_matrix
  })
  
  output$corr_plot <- renderPlot({
    M <- corr_data()
    
    validate(
      need(!any(is.na(M)), "Insufficient data to calculate correlations for this selection. Try selecting more countries.")
    )
    
    # Colorblind-friendly diverging palette
    # Blue (negative) -> White (zero) -> Orange (positive)
    # This palette works for all types of colorblindness (protanopia, deuteranopia, tritanopia)
    # Colors chosen to be distinguishable by brightness and hue even for colorblind users
    col <- colorRampPalette(c(
      "#2166AC",  # Dark blue (strong negative)
      "#4393C3",  # Medium blue
      "#92C5DE",  # Light blue
      "#D1E5F0",  # Very light blue
      "#FFFFFF",  # White (zero correlation)
      "#FDDBC7",  # Very light orange
      "#F4A582",  # Light orange
      "#D6604D",  # Medium orange-red
      "#B2182B"   # Dark red (strong positive)
    ))(200)
    
    corrplot(M, 
             method = "color", 
             type = "upper", 
             order = "hclust", 
             col = col,
             addCoef.col = "#0F172A", # Dark Slate for text (high contrast)
             tl.col = "#0F172A",      # Dark Slate for labels
             tl.srt = 45,             # Rotated labels
             diag = FALSE,            # Hide diagonal
             outline = TRUE,
             mar = c(0,0,1,0)
    )
  })
}
