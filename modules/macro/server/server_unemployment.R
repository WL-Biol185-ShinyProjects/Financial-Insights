# ============================================================================
# Server Logic: State Employment Data
# ============================================================================

unemployment_server <- function(input, output, session, macro_data, shared_state) {
  
  # Load Employment Data ----
  employment_data <- reactive({
    req(file.exists("data/state/raw_source/unemployment/JobOpens.xlsx"),
        file.exists("data/state/raw_source/unemployment/Hires.xlsx"),
        file.exists("data/state/raw_source/unemployment/Quits.xlsx"))
    
    # Read files
    jobs <- read_excel("data/state/raw_source/unemployment/JobOpens.xlsx")
    hires <- read_excel("data/state/raw_source/unemployment/Hires.xlsx")
    quits <- read_excel("data/state/raw_source/unemployment/Quits.xlsx")
    
    # Merge datasets
    df <- jobs %>%
      full_join(hires, by = "State") %>%
      full_join(quits, by = "State")
    
    # Filter out total U.S. row
    df <- df %>% filter(tolower(State) != "total u.s.")
    View(df)
    
    # Add state codes for plotly
    state_codes <- data.frame(
      State = tolower(state.name),
      State_Code = state.abb,
      stringsAsFactors = FALSE
    )
    
    df <- df %>%
      mutate(State_Lower = tolower(State)) %>%
      left_join(state_codes, by = c("State_Lower" = "State")) %>%
      select(-State_Lower)
    
    df
  })
  
  # Metric Explanation ----
  output$metric_explanation_employment <- renderText({
    metric <- input$map_metric_employment
    period <- input$time_period_employment
    
    period_label <- switch(
      period,
      "july_2025" = "July 2025",
      "june_2025" = "June 2025",
      "may_2025" = "May 2025",
      "april_2025" = "April 2025",
      "july_2024" = "July 2024"
    )
    
    switch(metric,
           "job_open_rate" = paste0("Unit: Percentage (%). Definition: Job openings as a percentage of total employment plus job openings. Data from ", period_label, ". Higher values indicate more job opportunities (shown in lighter blue)."),
           "job_open_level" = paste0("Unit: Thousands of jobs. Definition: Total number of job openings available. Data from ", period_label, ". Higher values indicate more job opportunities (shown in lighter blue)."),
           "hiring_rate" = paste0("Unit: Percentage (%). Definition: Hires as a percentage of total employment. Data from ", period_label, ". Higher values indicate stronger hiring activity (shown in lighter blue)."),
           "hiring_level" = paste0("Unit: Thousands of hires. Definition: Total number of workers hired during the period. Data from ", period_label, ". Higher values indicate stronger hiring activity (shown in lighter blue)."),
           "quitting_rate" = paste0("Unit: Percentage (%). Definition: Quits as a percentage of total employment. Data from ", period_label, ". Higher values may indicate worker confidence in finding new jobs (shown in lighter blue)."),
           "quitting_level" = paste0("Unit: Thousands of quits. Definition: Total number of workers who quit their jobs during the period. Data from ", period_label, ". Higher values may indicate worker confidence (shown in lighter blue).")
    )
  })
  
  # Employment Map ----
  output$employment_map_other <- renderPlotly({
    df <- employment_data()
    metric <- input$map_metric_employment
    period <- input$time_period_employment
    
    # Map period to column date string
    period_map <- list(
      "july_2025" = "7/1/2025",
      "june_2025" = "6/1/2025",
      "may_2025" = "5/1/2025",
      "april_2025" = "4/1/2025",
      "july_2024" = "7/1/2024"
    )
    date_str <- period_map[[period]]
    
    # Get column name based on metric and period
    metric_col <- switch(
      metric,
      "job_open_rate" = paste0("Job Open Rates -", date_str),
      "job_open_level" = paste0("Job Open Levels -", date_str),
      "hiring_rate" = paste0("Hiring Rates -", date_str),
      "hiring_level" = paste0("Hiring Levels -", date_str),
      "quitting_rate" = paste0("Quitting Rates -", date_str),
      "quitting_level" = paste0("Quitting Levels -", date_str)
    )
    
    metric_title <- switch(
      metric,
      "job_open_rate" = "Job Opening Rate",
      "job_open_level" = "Job Openings",
      "hiring_rate" = "Hiring Rate",
      "hiring_level" = "Hires",
      "quitting_rate" = "Quitting Rate",
      "quitting_level" = "Quits"
    )
    
    period_label <- switch(
      period,
      "july_2025" = "July 2025",
      "june_2025" = "June 2025",
      "may_2025" = "May 2025",
      "april_2025" = "April 2025",
      "july_2024" = "July 2024"
    )
    
    if (!metric_col %in% names(df)) {
      return(plot_ly() %>% layout(title = "Data not available for selected metric"))
    }
    
    # Create a column for the z values that plotly can access
    df$map_value <- df[[metric_col]]
    
    # Remove NA values for color scale calculation
    df_no_na <- df[!is.na(df$map_value), ]
    
    # Use percentile-based coloring for better distribution
    if (nrow(df_no_na) > 0) {
      df$map_value_normalized <- sapply(df$map_value, function(val) {
        if (is.na(val)) return(NA)
        percentile_rank <- sum(df_no_na$map_value <= val) / nrow(df_no_na)
        return(percentile_rank)
      })
      
      min_val <- 0
      max_val <- 1
    }
    
    # Create properly formatted hover text with units
    df$hover_text <- sapply(seq_len(nrow(df)), function(i) {
      value <- df[[metric_col]][i]
      if (is.na(value)) {
        formatted_value <- "Data Not Available"
      } else if (metric %in% c("job_open_rate", "hiring_rate", "quitting_rate")) {
        formatted_value <- paste0(round(value, 1), "%")
      } else {
        formatted_value <- paste0(format(round(value), big.mark = ","), "k")
      }
      
      paste0(
        "<b>", df$State[i], "</b><br>",
        metric_title, " (", period_label, "): ", formatted_value
      )
    })
    
    # Color scale (matching your app's blue theme)
    color_scale <- list(
      c(0, "#0f172a"),
      c(0.25, "#1e40af"),
      c(0.5, "#2563eb"),
      c(0.75, "#60a5fa"),
      c(1, "#dbeafe")
    )
    
    plot_geo(
      data = df,
      locationmode = "USA-states"
    ) %>%
      add_trace(
        locations = ~State_Code,
        z = ~map_value_normalized,
        text = ~hover_text,
        hoverinfo = "text",
        type = "choropleth",
        colorscale = color_scale,
        colorbar = list(
          title = list(
            text = metric_title,
            side = "right"
          ),
          tickmode = "array",
          tickvals = c(0, 0.25, 0.5, 0.75, 1),
          ticktext = c("Low", "", "Medium", "", "High")
        ),
        zmin = if (nrow(df_no_na) > 0) min_val else NULL,
        zmax = if (nrow(df_no_na) > 0) max_val else NULL
      ) %>%
      layout(
        title = list(
          text = paste("U.S. State-Level", metric_title, "-", period_label),
          font = list(size = 16, color = "#1e293b")
        ),
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white"),
          bgcolor = "#f8fafc"
        ),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#ffffff"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Top States Table ----
  output$top_states_employment <- renderTable({
    df <- employment_data()
    metric <- input$map_metric_employment
    period <- input$time_period_employment
    
    period_map <- list(
      "july_2025" = "7/1/2025",
      "june_2025" = "6/1/2025",
      "may_2025" = "5/1/2025",
      "april_2025" = "4/1/2025",
      "july_2024" = "7/1/2024"
    )
    date_str <- period_map[[period]]
    
    metric_col <- switch(
      metric,
      "job_open_rate" = paste0("Job Open Rates -", date_str),
      "job_open_level" = paste0("Job Open Levels -", date_str),
      "hiring_rate" = paste0("Hiring Rates -", date_str),
      "hiring_level" = paste0("Hiring Levels -", date_str),
      "quitting_rate" = paste0("Quitting Rates -", date_str),
      "quitting_level" = paste0("Quitting Levels -", date_str)
    )
    
    if (!metric_col %in% names(df)) {
      return(data.frame(Rank = "N/A", State = "N/A", Value = "N/A"))
    }
    
    sorted <- df %>% 
      filter(!is.na(!!sym(metric_col))) %>%
      arrange(desc(!!sym(metric_col))) %>% 
      head(5)
    
    result <- sorted %>%
      select(State, Value = !!sym(metric_col)) %>%
      mutate(Rank = row_number())
    
    if (metric %in% c("job_open_rate", "hiring_rate", "quitting_rate")) {
      result <- result %>% mutate(Value = paste0(round(Value, 1), "%"))
    } else {
      result <- result %>% mutate(Value = paste0(format(round(Value), big.mark = ","), "k"))
    }
    
    result %>% select(Rank, State, Value)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # Bottom States Table ----
  output$bottom_states_employment <- renderTable({
    df <- employment_data()
    metric <- input$map_metric_employment
    period <- input$time_period_employment
    
    period_map <- list(
      "july_2025" = "7/1/2025",
      "june_2025" = "6/1/2025",
      "may_2025" = "5/1/2025",
      "april_2025" = "4/1/2025",
      "july_2024" = "7/1/2024"
    )
    date_str <- period_map[[period]]
    
    metric_col <- switch(
      metric,
      "job_open_rate" = paste0("Job Open Rates -", date_str),
      "job_open_level" = paste0("Job Open Levels -", date_str),
      "hiring_rate" = paste0("Hiring Rates -", date_str),
      "hiring_level" = paste0("Hiring Levels -", date_str),
      "quitting_rate" = paste0("Quitting Rates -", date_str),
      "quitting_level" = paste0("Quitting Levels -", date_str)
    )
    
    if (!metric_col %in% names(df)) {
      return(data.frame(Rank = "N/A", State = "N/A", Value = "N/A"))
    }
    
    sorted <- df %>% 
      filter(!is.na(!!sym(metric_col))) %>%
      arrange(!!sym(metric_col)) %>% 
      head(5)
    
    result <- sorted %>%
      select(State, Value = !!sym(metric_col)) %>%
      mutate(Rank = row_number())
    
    if (metric %in% c("job_open_rate", "hiring_rate", "quitting_rate")) {
      result <- result %>% mutate(Value = paste0(round(Value, 1), "%"))
    } else {
      result <- result %>% mutate(Value = paste0(format(round(Value), big.mark = ","), "k"))
    }
    
    result %>% select(Rank, State, Value)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
}