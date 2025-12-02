# ============================================================================
# Commodity Prices Comparison - UI Component
# ============================================================================

commodity_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Indicator vs Commodity Prices"),
        p("Compare how U.S. macroeconomic indicators correlate with gold or oil prices over time. ",
          "Gold prices are in USD per troy ounce. Oil prices are WTI crude oil in USD per barrel. ",
          "This analysis is limited to United States data only. ",
          "Data sources: DataHub.io (gold prices), FRED Economic Data (oil prices).")
      )
    ),
    column(4,
      card(
        card_header("Controls"),
        # Commodity Selection
        selectInput("commodity_type", "Select Commodity:", 
                   choices = c("Gold Prices" = "gold", 
                             "Oil Prices (WTI)" = "oil"),
                   selected = "gold"),
        
        # Indicator Selection
        selectInput("commodity_indicator", "Economic Indicator:", 
                   choices = c("GDP per Capita" = "gdp_per_capita", 
                             "Inflation" = "inflation", 
                             "Unemployment" = "unemployment", 
                             "Life Expectancy" = "life_expectancy",
                             "Govt Debt" = "govt_debt",
                             "GDP" = "gdp",
                             "Population Growth" = "pop_growth"),
                   selected = "gdp_per_capita"),
        
        # Year slider and play/pause button
        fluidRow(
          column(8,
            sliderInput("commodity_year", "Year:", min = 1960, max = 2023, value = 2000, sep = "", 
                       animate = FALSE, width = "100%")
          ),
          column(4,
            br(),
            actionButton("commodity_play_pause", "Play", icon = shiny::icon("play"), 
                        class = "btn-primary", style = "width: 100%; margin-top: 5px;")
          )
        ),
        div(style = "font-size: 12px; color: #64748b; margin-top: 5px;",
          textOutput("commodity_animation_status", inline = TRUE)
        )
      )
    ),
    column(8,
      card(
        card_header("Indicator vs Commodity Prices Comparison"),
        plotlyOutput("commodity_plot", height = "500px")
      ),
      card(
        card_header("Correlation Analysis"),
        htmlOutput("commodity_correlation")
      )
    )
  )
}

