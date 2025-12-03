# ============================================================================
# Relationships - UI Component
# ============================================================================

relationships_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Indicator Relationships"),
        p("Compare how two different economic indicators evolve together over time for a specific country.")
      )
    ),
    column(4,
      card(
        card_header("Controls"),
        # Select Single Country - Container will expand when opened
        selectizeInput("rel_country", "Select Country:", choices = NULL, multiple = FALSE, 
                       options = list(placeholder = "Type to search...")),
        
        # Indicator 1 (Left Axis)
        selectInput("rel_y1", "Indicator 1 (Left Axis):", 
                   choices = c("GDP per Capita" = "gdp_per_capita", 
                             "Inflation" = "inflation", 
                             "Unemployment" = "unemployment", 
                             "Life Expectancy" = "life_expectancy",
                             "Govt Debt" = "govt_debt")),
                             
        # Indicator 2 (Right Axis)
        selectInput("rel_y2", "Indicator 2 (Right Axis):", 
                   choices = c("Inflation" = "inflation", 
                             "GDP per Capita" = "gdp_per_capita", 
                             "Unemployment" = "unemployment", 
                             "Life Expectancy" = "life_expectancy",
                             "Govt Debt" = "govt_debt"),
                   selected = "inflation"),
        
        # Year Animation Controls
        hr(),
        h5("Year Animation", style = "margin-top: 10px; margin-bottom: 10px;"),
        sliderInput("rel_year", "Year:", min = 1960, max = 2023, value = 2023, 
                   step = 1, sep = "", animate = FALSE),
        div(style = "display: flex; align-items: center; gap: 10px; margin-top: 10px;",
          actionButton("rel_play_pause", "Play", icon = shiny::icon("play"), 
                      class = "btn-primary", style = "flex: 0 0 auto;"),
          textOutput("rel_animation_status", inline = TRUE)
        )
      )
    ),
    column(8,
      card(
        card_header("Dual-Axis Comparison"),
        plotlyOutput("rel_plot", height = "500px")
      )
    )
  )
}
