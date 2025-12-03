# ============================================================================
# Time Series Analysis - Server Logic
# Module 1: Global Macroeconomic Explorer
# ============================================================================

time_series_server <- function(input, output, session, macro_data, shared_state) {
  
  # Animation state
  is_playing <- reactiveVal(FALSE)
  
  # Get year range from data
  year_range <- reactive({
    req(macro_data)
    years <- sort(unique(macro_data$year))
    list(min = min(years, na.rm = TRUE), max = max(years, na.rm = TRUE), all = years)
  })
  
  # Initialize country selector with available countries
  observe({
    countries <- unique(macro_data$country) %>% sort()
    updateSelectizeInput(
      session,
      "ts_countries",
      choices = countries,
      selected = c("United States", "China", "Germany")
    )
  })
  
  # Initialize year slider
  observe({
    years <- year_range()$all
    updateSliderInput(session, "ts_year", min = min(years), max = max(years), 
                     value = max(years, na.rm = TRUE))
  })
  
  # Play/Pause button handler
  observeEvent(input$ts_play_pause, {
    if (is_playing()) {
      # Pause animation
      is_playing(FALSE)
      updateActionButton(session, "ts_play_pause", label = "Play", icon = shiny::icon("play"))
    } else {
      # Start animation
      is_playing(TRUE)
      updateActionButton(session, "ts_play_pause", label = "Pause", icon = shiny::icon("pause"))
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
    current_year <- input$ts_year
    current_idx <- which(years == current_year)
    
    if (length(current_idx) == 0) {
      current_idx <- 1
    }
    
    # Move to next year
    if (current_idx < length(years)) {
      next_year <- years[current_idx + 1]
      updateSliderInput(session, "ts_year", value = next_year)
    } else {
      # Reached the end, stop animation
      is_playing(FALSE)
      updateActionButton(session, "ts_play_pause", label = "Play", icon = shiny::icon("play"))
    }
  })
  
  # Animation status text
  output$ts_animation_status <- renderText({
    if (is_playing()) {
      years <- year_range()$all
      current_idx <- which(years == input$ts_year)
      total <- length(years)
      paste("Playing:", current_idx, "of", total, "years")
    } else {
      paste("Year:", input$ts_year)
    }
  })
  
  # Reactive filtered data - Auto-updates on input change
  ts_data <- reactive({
    req(input$ts_countries, input$ts_year, input$ts_indicator)
    
    # Update shared state for other modules
    shared_state$selected_countries <- input$ts_countries
    shared_state$year_range <- c(year_range()$min, input$ts_year)
    
    macro_data %>%
      filter(
        country %in% input$ts_countries,
        year <= input$ts_year,
        !is.na(.data[[input$ts_indicator]])
      ) %>%
      arrange(year) # Ensure sorted by year
  })
  
  # Quick Stats
  output$ts_quick_stats <- renderUI({
    data <- ts_data()
    req(data)
    
    n_countries <- n_distinct(data$country)
    n_years <- n_distinct(data$year)
    n_obs <- nrow(data)
    
    tagList(
      tags$p(tags$b("Countries:"), n_countries, style = "margin: 5px 0;"),
      tags$p(tags$b("Years:"), paste(min(data$year), "-", max(data$year)), style = "margin: 5px 0;"),
      tags$p(tags$b("Data Points:"), n_obs, style = "margin: 5px 0;")
    )
  })
  
  # Time Series Plot
  output$ts_plot <- renderPlotly({
    data <- ts_data()
    req(data, nrow(data) > 0)
    
    # Get indicator name
    indicator_names <- c(
      "gdp_per_capita" = "GDP per Capita (constant 2015 USD)",
      "inflation" = "Inflation Rate (%)",
      "unemployment" = "Unemployment Rate (%)",
      "life_expectancy" = "Life Expectancy (years)",
      "pop_growth" = "Population Growth (%)"
    )
    
    indicator_name <- indicator_names[input$ts_indicator]
    
    # Modern Financial Palette
    prof_palette <- c("#0F172A", "#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444", "#06B6D4", "#EC4899")
    
    # Dynamic Palette Logic
    n_colors <- length(unique(data$country))
    final_palette <- if(n_colors <= length(prof_palette)) prof_palette[1:n_colors] else colorRampPalette(prof_palette)(n_colors)
    
    # Create plot
    p <- ggplot(data, aes(x = year, y = .data[[input$ts_indicator]], 
                          color = country, group = country)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2.5, alpha = 0.9) +
      scale_color_manual(values = final_palette) +
      labs(
        title = paste(indicator_name, "Over Time"),
        x = "Year",
        y = indicator_name,
        color = "Country"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#1e293b"),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        axis.text = element_text(color = "#64748b"),
        axis.title = element_text(color = "#475569")
      )
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "closest")
  })
  
  # Summary Statistics Table
  output$ts_summary_table <- renderDT({
    data <- ts_data()
    req(data)
    
    summary_data <- data %>%
      group_by(country) %>%
      summarise(
        `Avg Value` = mean(.data[[input$ts_indicator]], na.rm = TRUE),
        `Min Value` = min(.data[[input$ts_indicator]], na.rm = TRUE),
        `Max Value` = max(.data[[input$ts_indicator]], na.rm = TRUE),
        `Std Dev` = sd(.data[[input$ts_indicator]], na.rm = TRUE),
        `Data Points` = n(),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
}
