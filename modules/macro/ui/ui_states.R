# ============================================================================
# U.S. States Analysis - UI Component
# ============================================================================

states_ui <- function() {
  fluidRow(
    column(12,
      card(
        card_header("Which States Have Better Economic Health?"),
        div(class = "p-3",
          p(tags$b("This tool compares all 50 U.S. states on key economic measures:")),
          tags$ul(
            tags$li(tags$b("Income:"), " How much do households earn? (median = middle value)"),
            tags$li(tags$b("Jobs:"), " How many people are unemployed and looking for work?"),
            tags$li(tags$b("Poverty:"), " How many people live below the poverty line?"),
            tags$li(tags$b("Cost of Living:"), " How expensive is it to live there compared to the U.S. average?")
          )
        )
      )
    ),
    
    column(3,
      card(
        card_header("Map Controls"),
        selectizeInput("states_map_metric", "Select Metric to Display:",
                      choices = c(
                        "Median Household Income" = "median_income",
                        "Unemployment Rate" = "unemployment",
                        "Poverty Rate" = "poverty",
                        "Cost of Living Index" = "cost_living"
                      ),
                      selected = "median_income",
                      options = list(dropdownParent = 'body')),
        p(style = "font-size: 12px; color: #64748b; margin-top: 10px;",
          textOutput("states_metric_explanation"))
      ),
      
      card(
        card_header("Pick Any Two States to Compare"),
        p(style = "font-size:13px; color:#64748b;",
          "Select two states below to see their economic data side-by-side:"),
        selectizeInput("states_compare_state_1", "First State:",
                      choices = NULL,
                      options = list(dropdownParent = 'body')),
        selectizeInput("states_compare_state_2", "Second State:",
                      choices = NULL,
                      options = list(dropdownParent = 'body')),
        actionButton("states_compare_btn", "Show Comparison",
                    class = "btn-primary", style = "width:100%;")
      ),
      
      card(
        card_header("Top 5 Best States"),
        p(style = "font-size:12px; color:#64748b; margin-bottom:10px;",
          "States with the best (highest/lowest) values for selected metric:"),
        tableOutput("states_top_table")
      ),
      
      card(
        card_header("Bottom 5 Worst States"),
        p(style = "font-size:12px; color:#64748b; margin-bottom:10px;",
          "States with the worst (lowest/highest) values for selected metric:"),
        tableOutput("states_bottom_table")
      )
    ),
    
    column(9,
      card(
        card_header("Interactive State Map"),
        plotlyOutput("states_map", height = "500px")
      ),
      
      card(
        card_header("Side-by-Side State Comparison"),
        p(style = "font-size:13px; color:#64748b; margin-bottom:10px;",
          "Select two states from the left sidebar and click 'Show Comparison' to see detailed data:"),
        uiOutput("states_comparison_output")
      )
    )
  )
}
