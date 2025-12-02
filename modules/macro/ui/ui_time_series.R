# ============================================================================
# Time Series Analysis - UI Component
# ============================================================================

time_series_ui <- function() {
  fluidRow(
    # Page Header
    column(12,
      card(
        card_header("Time Series Analysis"),
        p("Visualize how economic indicators evolve over time across multiple countries.")
      )
    ),
    
    # Controls Sidebar - Wider for better dropdown visibility
    column(4,
      card(
        card_header("Controls"),
        selectizeInput("ts_countries", "Select Countries:", choices = NULL, multiple = TRUE, 
                      options = list(placeholder = "Choose countries...", maxItems = 5)),
        div(class = "alert alert-info", style = "padding: 8px; margin-top: 5px; margin-bottom: 15px; font-size: 12px;",
            bs_icon("info-circle"), " You can select up to 5 countries for comparison."),
        sliderInput("ts_year_range", "Year Range:", min = 1960, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
        selectInput("ts_indicator", "Select Indicator:", choices = list("GDP per Capita" = "gdp_per_capita", "Inflation Rate (%)" = "inflation", "Unemployment Rate (%)" = "unemployment", "Life Expectancy (years)" = "life_expectancy", "Population Growth (%)" = "pop_growth"), selected = "gdp_per_capita"),
        hr(),
        div(style = "background-color: #f9f9f9; padding: 10px; border-radius: 3px; border: 1px solid #eee;",
          h5("Quick Stats", style = "margin-top: 0;"),
          uiOutput("ts_quick_stats")
        )
      )
    ),
    
    # Main Plot Area - Adjusted width
    column(8,
      card(
        card_header("Chart"),
        plotlyOutput("ts_plot", height = "500px")
      ),
      card(
        card_header("Summary Statistics"),
        DTOutput("ts_summary_table")
      )
    )
  )
}
