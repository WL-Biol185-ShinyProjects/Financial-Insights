# ============================================================================
# Regional Trends - Server Logic
# ============================================================================

regional_trends_server <- function(input, output, session, macro_data, shared_state) {
  
  # Animation state
  is_playing <- reactiveVal(FALSE)
  
  # Get year range from data
  year_range <- reactive({
    req(macro_data)
    years <- sort(unique(macro_data$year))
    list(min = min(years, na.rm = TRUE), max = max(years, na.rm = TRUE), all = years)
  })
  
  # Play/Pause button handler
  observeEvent(input$reg_play_pause, {
    if (is_playing()) {
      # Pause animation
      is_playing(FALSE)
      updateActionButton(session, "reg_play_pause", label = "Play", icon = shiny::icon("play"))
    } else {
      # Start animation
      is_playing(TRUE)
      updateActionButton(session, "reg_play_pause", label = "Pause", icon = shiny::icon("pause"))
      # Trigger the animation observe block immediately
      invalidateLater(0, session)
    }
  })
  
  # Animation logic - triggered by timer when playing
  observe({
    # Only proceed if playing
    if (!is_playing()) return()
    
    years <- year_range()$all
    current_year <- input$reg_year
    current_idx <- which(years == current_year)
    
    if (length(current_idx) == 0) {
      current_idx <- 1
    }
    
    # Move to next year
    if (current_idx < length(years)) {
      next_year <- years[current_idx + 1]
      updateSliderInput(session, "reg_year", value = next_year)
    } else {
      # Reached the end, stop animation
      is_playing(FALSE)
      updateActionButton(session, "reg_play_pause", label = "Play", icon = shiny::icon("play"))
      return()
    }
    
    # Re-schedule for next iteration (500ms delay)
    invalidateLater(500, session)
  })
  
  # Reset to first year when indicator changes
  observeEvent(input$reg_indicator, {
    if (is_playing()) {
      is_playing(FALSE)
      updateActionButton(session, "reg_play_pause", label = "Play", icon = shiny::icon("play"))
    }
    years <- year_range()$all
    if (length(years) > 0) {
      updateSliderInput(session, "reg_year", value = years[1])
    }
  })
  
  # Animation status text
  output$reg_animation_status <- renderText({
    if (is_playing()) {
      years <- year_range()$all
      current_idx <- which(years == input$reg_year)
      total <- length(years)
      paste("Playing:", current_idx, "of", total, "years")
    } else {
      ""
    }
  })
  
  # Reactive regional data - Auto-updates on input change
  # Shows data from start year up to selected year (for animation effect)
  regional_data_reactive <- reactive({
    req(input$reg_indicator, input$reg_year)
    
    indicator <- input$reg_indicator
    end_year <- input$reg_year
    start_year <- year_range()$min
    
    # Calculate regional averages (weighted by population)
    # Filter to show data from start up to selected year
    macro_data %>%
      filter(year >= start_year, year <= end_year, !is.na(.data[[indicator]]), !is.na(population)) %>%
      group_by(region, year) %>%
      summarise(
        value = weighted.mean(.data[[indicator]], w = population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(region, year) # Sort for clean plotting
  })
  
  output$regional_plot <- renderPlotly({
    data <- regional_data_reactive()
    indicator <- input$reg_indicator
    
    validate(
      need(nrow(data) > 0, "No data available for selected parameters.")
    )
    
    indicator_label <- tools::toTitleCase(gsub("_", " ", indicator))
    
    # Modern Financial Palette
    prof_palette <- c("#0F172A", "#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444", "#06B6D4", "#EC4899")
    
    # Dynamic Palette Logic
    n_colors <- length(unique(data$region))
    final_palette <- if(n_colors <= length(prof_palette)) prof_palette[1:n_colors] else colorRampPalette(prof_palette)(n_colors)
    
    p <- ggplot(data, aes(x = year, y = value, color = region)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      scale_color_manual(values = final_palette) +
      labs(
        title = paste("Regional Trends:", indicator_label, "-", input$reg_year),
        x = "Year",
        y = paste("Avg", indicator_label),
        color = "Region"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16, color = "#1e293b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        axis.text = element_text(color = "#64748b"),
        axis.title = element_text(color = "#475569")
      )
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
}
