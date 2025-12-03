# ============================================================================
# Global Map - Server Logic
# ============================================================================

global_map_server <- function(input, output, session, macro_data, shared_state) {
  
  # Animation state
  is_playing <- reactiveVal(FALSE)
  
  # Track multiple indicators - each has an ID and selected indicator value
  indicators <- reactiveValues(
    count = 1,
    data = list()
  )
  
  # Initialize with one indicator
  observe({
    if (length(indicators$data) == 0) {
      indicators$data[[1]] <- list(id = 1, indicator = "gdp_per_capita")
    }
  })
  
  # Indicator choices
  indicator_choices <- list(
    "GDP per Capita" = "gdp_per_capita",
    "Inflation Rate" = "inflation",
    "Unemployment Rate" = "unemployment",
    "Life Expectancy" = "life_expectancy",
    "Population Growth" = "pop_growth"
  )
  
  # Get year range from data
  year_range <- reactive({
    req(macro_data)
    years <- sort(unique(macro_data$year))
    list(min = min(years, na.rm = TRUE), max = max(years, na.rm = TRUE), all = years)
  })
  
  # Add new indicator
  observeEvent(input$map_add_indicator, {
    new_id <- indicators$count + 1
    indicators$count <- new_id
    indicators$data[[new_id]] <- list(id = new_id, indicator = "gdp_per_capita")
  })
  
  # Remove indicator button handler
  observe({
    lapply(seq_along(indicators$data), function(i) {
      id <- indicators$data[[i]]$id
      remove_id <- paste0("map_remove_", id)
      if (!is.null(input[[remove_id]])) {
        if (input[[remove_id]] > 0) {
          # Remove this indicator by filtering it out
          indicators$data <- indicators$data[sapply(indicators$data, function(x) x$id != id)]
        }
      }
    })
  })
  
  # Update indicator when user changes selection
  observe({
    lapply(seq_along(indicators$data), function(i) {
      id <- indicators$data[[i]]$id
      select_id <- paste0("map_indicator_", id)
      if (!is.null(input[[select_id]])) {
        indicators$data[[i]]$indicator <- input[[select_id]]
      }
    })
  })
  
  # Play/Pause button handler
  observeEvent(input$map_play_pause, {
    if (is_playing()) {
      # Pause animation
      is_playing(FALSE)
      updateActionButton(session, "map_play_pause", label = "Play", icon = shiny::icon("play"))
    } else {
      # Start animation
      is_playing(TRUE)
      updateActionButton(session, "map_play_pause", label = "Pause", icon = shiny::icon("pause"))
    }
  })
  
  # Animation speed (milliseconds between updates)
  animation_speed <- reactive({
    as.numeric(input$map_speed)
  })
  
  # Animation logic - triggered by timer when playing
  observe({
    # Only proceed if playing
    if (!is_playing()) return()
    
    # Invalidate after the specified delay
    invalidateLater(animation_speed(), session)
    
    years <- year_range()$all
    current_year <- input$map_year
    current_idx <- which(years == current_year)
    
    if (length(current_idx) == 0) {
      current_idx <- 1
    }
    
    # Move to next year
    if (current_idx < length(years)) {
      next_year <- years[current_idx + 1]
      updateSliderInput(session, "map_year", value = next_year)
    } else {
      # Reached the end, stop animation
      is_playing(FALSE)
      updateActionButton(session, "map_play_pause", label = "Play", icon = shiny::icon("play"))
    }
  })
  
  # Animation status text
  output$map_animation_status <- renderText({
    if (is_playing()) {
      years <- year_range()$all
      current_idx <- which(years == input$map_year)
      total <- length(years)
      paste("Playing:", current_idx, "of", total, "years")
    } else {
      ""
    }
  })
  
  # Helper function to create a single map plot
  create_map_plot <- function(map_data, indicator, year) {
    indicator_label <- tools::toTitleCase(gsub("_", " ", indicator))
    
    # Dynamic color scale based on actual data range
    data_values <- map_data[[indicator]]
    data_min <- min(data_values, na.rm = TRUE)
    data_max <- max(data_values, na.rm = TRUE)
    
    # For GDP per capita, use a more reasonable scale
    if (indicator == "gdp_per_capita") {
      scale_max <- quantile(data_values, 0.95, na.rm = TRUE)
      scale_min <- data_min
    } else {
      scale_max <- data_max
      scale_min <- data_min
    }
    
    # Modern Light Theme Map with Dynamic Scale
    plot_geo(map_data) %>%
      add_trace(
        z = ~get(indicator),
        color = ~get(indicator),
        colors = "Blues",
        text = ~paste(country, "<br>", indicator_label, ":", round(get(indicator), 2)),
        locations = ~iso3c,
        marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5)),
        zmin = scale_min,
        zmax = scale_max
      ) %>%
      colorbar(title = indicator_label, len = 0.9) %>%
      layout(
        title = list(text = paste("Global", indicator_label, "-", year), font = list(color = "#1e293b", size = 18)),
        font = list(family = "Inter"),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#e2e8f0",
          projection = list(type = 'natural earth'),
          bgcolor = "rgba(0,0,0,0)",
          lakecolor = "#f1f5f9",
          landcolor = "#f8fafc",
          countrycolor = "#e2e8f0"
        ),
        margin = list(t = 50, b = 0, l = 0, r = 0)
      )
  }
  
  # Dynamically render UI for all indicators
  output$map_indicators_ui <- renderUI({
    req(length(indicators$data) > 0)
    
    map_list <- lapply(seq_along(indicators$data), function(i) {
      id <- indicators$data[[i]]$id
      current_indicator <- indicators$data[[i]]$indicator
      
      # Determine column width based on number of maps
      num_maps <- length(indicators$data)
      col_width <- if (num_maps == 1) 12 else if (num_maps == 2) 6 else 4
      
      column(
        width = col_width,
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste("Map", id)),
              if (length(indicators$data) > 1) {
                actionButton(
                  paste0("map_remove_", id),
                  "",
                  icon = shiny::icon("times"),
                  class = "btn-sm btn-danger",
                  style = "padding: 2px 6px;"
                )
              } else NULL
            )
          ),
          selectInput(
            paste0("map_indicator_", id),
            "Indicator:",
            choices = indicator_choices,
            selected = current_indicator
          ),
          plotlyOutput(
            paste0("map_plot_", id),
            height = "500px"
          )
        )
      )
    })
    
    do.call(fluidRow, map_list)
  })
  
  # Create reactive expressions for map data for each indicator
  # We'll create outputs dynamically by watching the indicators list
  map_outputs_created <- reactiveValues(ids = character(0))
  
  # Dynamically create renderPlotly outputs for each map
  observe({
    req(length(indicators$data) > 0)
    
    current_ids <- sapply(indicators$data, function(x) paste0("map_plot_", x$id))
    
    # Create outputs for new indicators
    for (i in seq_along(indicators$data)) {
      id <- indicators$data[[i]]$id
      output_id <- paste0("map_plot_", id)
      
      if (!output_id %in% map_outputs_created$ids) {
        local({
          my_id <- id
          my_output_id <- output_id
          
          output[[my_output_id]] <- renderPlotly({
            req(input$map_year)
            
            # Make this reactive to indicators$data changes
            indicators_list <- indicators$data
            
            # Find the current indicator for this map
            current_ind <- NULL
            for (j in seq_along(indicators_list)) {
              if (indicators_list[[j]]$id == my_id) {
                current_ind <- indicators_list[[j]]$indicator
                break
              }
            }
            
            req(current_ind)
            
            map_data <- macro_data %>%
              filter(year == input$map_year, !is.na(.data[[current_ind]]))
            
            validate(
              need(nrow(map_data) > 0, paste("No data available for", current_ind, "in", input$map_year))
            )
            
            create_map_plot(map_data, current_ind, input$map_year)
          })
          
          outputOptions(output, my_output_id, suspendWhenHidden = FALSE)
        })
        
        map_outputs_created$ids <- c(map_outputs_created$ids, output_id)
      }
    }
    
    # Remove outputs for deleted indicators
    map_outputs_created$ids <- intersect(map_outputs_created$ids, current_ids)
  })
}
