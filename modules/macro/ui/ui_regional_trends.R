# ============================================================================
# Regional Trends - UI Component
# ============================================================================

regional_trends_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Regional Trends"),
        p("Compare economic trends across world regions over time (population-weighted averages).")
      )
    ),
    column(4,
      card(
        card_header("Controls"),
        selectInput("reg_indicator", "Indicator:", choices = list("GDP per Capita" = "gdp_per_capita", "Inflation Rate" = "inflation", "Unemployment Rate" = "unemployment", "Life Expectancy" = "life_expectancy", "Population Growth" = "pop_growth"), selected = "gdp_per_capita"),
        sliderInput("reg_year", "Year:", min = 1960, max = 2023, value = 1960, sep = "", animate = FALSE),
        selectInput("reg_speed", "Animation Speed:", 
                   choices = c("Very Slow" = 2000, "Slow" = 1000, "Normal" = 500, "Fast" = 250, "Very Fast" = 100),
                   selected = 500, width = "100%"),
        div(style = "margin-top: 15px;",
          actionButton("reg_play_pause", "Play", icon = shiny::icon("play"), class = "btn-primary", style = "width: 100%;"),
          div(style = "margin-top: 10px; font-size: 12px; color: #64748b; text-align: center;",
            textOutput("reg_animation_status")
          )
        )
      )
    ),
    column(8,
      card(
        card_header("Regional Comparison"),
        plotlyOutput("regional_plot", height = "500px")
      )
    )
  )
}
