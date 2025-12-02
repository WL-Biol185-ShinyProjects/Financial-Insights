# ============================================================================
# Correlations - UI Component
# ============================================================================

correlations_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Correlation Matrix"),
        p("Explore correlations between different economic indicators. Select specific countries to see how their economic variables interact over time.")
      )
    ),
    column(4,
      card(
        card_header("Filters"),
        # Container will expand when opened
        selectizeInput("corr_countries", "Filter by Country:", choices = NULL, multiple = TRUE, 
                      options = list(placeholder = "All Countries (Global Analysis)")),
        
        # Year slider and play/pause button
        fluidRow(
          column(8,
            sliderInput("corr_year", "Year:", min = 1960, max = 2023, value = 2000, sep = "", 
                       animate = FALSE, width = "100%")
          ),
          column(4,
            br(),
            actionButton("corr_play_pause", "Play", icon = shiny::icon("play"), 
                        class = "btn-primary", style = "width: 100%; margin-top: 5px;")
          )
        ),
        div(style = "font-size: 12px; color: #64748b; margin-top: 5px;",
          textOutput("corr_animation_status", inline = TRUE)
        )
      )
    ),
    column(8,
      card(
        card_header("Heatmap"),
        plotOutput("corr_plot", height = "600px")
      )
    )
  )
}
