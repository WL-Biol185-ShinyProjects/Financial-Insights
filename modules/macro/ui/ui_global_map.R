# ============================================================================
# Global Map - UI Component
# ============================================================================

global_map_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Global Map"),
        p("Interactive world map showing economic indicators by country. Add multiple indicators to compare them side by side.")
      )
    ),
    column(12,
      card(
        card_header("Map Controls"),
        fluidRow(
          column(6,
            sliderInput("map_year", "Year:", min = 1960, max = 2023, value = 1960, sep = "", animate = FALSE)
          ),
          column(3,
            div(style = "margin-top: 25px;",
              actionButton("map_play_pause", "Play", icon = shiny::icon("play"), class = "btn-primary", style = "width: 100%;"),
              div(style = "margin-top: 10px; font-size: 12px; color: #64748b; text-align: center;",
                textOutput("map_animation_status")
              )
            )
          ),
          column(3,
            div(style = "margin-top: 25px;",
              actionButton("map_add_indicator", "Add Indicator", icon = shiny::icon("plus"), class = "btn-success", style = "width: 100%;")
            )
          )
        )
      )
    ),
    column(12,
      uiOutput("map_indicators_ui")
    )
  )
}
