# ============================================================================
# Retirement Simulator - UI Component (Auto-Update System)
# ============================================================================

renderInputs <- function(prefix) {
  div(
    style = "border: 2px solid #e2e8f0; border-radius: 8px; padding: 20px; margin-bottom: 15px;",
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_years"), "Number of Years:",
                         min = 5, max = 40, value = 20, step = 1),
             sliderInput(paste0(prefix, "_startAmount"), "Starting Amount:",
                         min = 100000, max = 10000000, value = 2000000,
                         step = 50000, pre = "$", sep = ","),
             sliderInput(paste0(prefix, "_growthRate"), "Annual Investment Return (%):",
                         min = 0.0, max = 30.0, value = 5.0, step = 0.5),
             sliderInput(paste0(prefix, "_standardDev"), "Investment Volatility (%):",
                         min = 0.0, max = 30.0, value = 7.0, step = 0.5)
      ),
      column(6,
             sliderInput(paste0(prefix, "_inflation"), "Annual Inflation (%):",
                         min = 0, max = 20, value = 2.5, step = 0.1),
             sliderInput(paste0(prefix, "_infStandardDev"), "Inflation Volatility (%):",
                         min = 0.0, max = 5.0, value = 1.5, step = 0.1),
             sliderInput(paste0(prefix, "_withdrawals"), "Monthly Withdrawals:",
                         min = 1000, max = 100000, value = 10000,
                         step = 1000, pre = "$", sep = ",")
      )
    )
  )
}

simulator_ui <- function() {
  fluidRow(
    column(12,
           card(
             card_header("Retirement Planning: Monte Carlo Simulation"),
             p("Compare two retirement scenarios side-by-side. Each simulation runs 200 scenarios based on random market returns and inflation. Results update automatically as you adjust the parameters.")
           )
    ),
    
    # Scenario A
    column(12,
           card(
             card_header("Scenario A - Monte Carlo Retirement Simulation"),
             renderInputs("case_a_for_retirement"),
             div(style = "border: 2px solid #e2e8f0; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
                 h5("Scenario A Results", style = "margin-top: 0;"),
                 verbatimTextOutput("a_summary")
             ),
             div(style = "border: 2px solid #e2e8f0; border-radius: 8px; padding: 15px;",
                 plotOutput("a_distPlot", height = "500px")
             )
           )
    ),
    
    # Scenario B
    column(12,
           card(
             card_header("Scenario B - Monte Carlo Retirement Simulation"),
             renderInputs("case_b_for_retirement"),
             div(style = "border: 2px solid #e2e8f0; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
                 h5("Scenario B Results", style = "margin-top: 0;"),
                 verbatimTextOutput("b_summary")
             ),
             div(style = "border: 2px solid #e2e8f0; border-radius: 8px; padding: 15px;",
                 plotOutput("b_distPlot", height = "500px")
             )
           )
    ),
    
    # Explanatory text
    column(12,
           card(
             card_header("How to Read the Charts"),
             div(style = "color: #64748b; font-size: 14px; line-height: 1.8;",
                 p(strong("Left Plot:"), "Displays 200 possible portfolio value trajectories over time. Each line represents one simulation outcome based on random market returns and inflation."),
                 p(strong("Right Plot:"), "Shows the success rate over time - the percentage of simulations where money is still available at each point."),
                 p(strong("Success Rate Interpretation:"),
                   tags$ul(
                     tags$li(strong("Above 75%:"), " Generally considered safe for retirement planning"),
                     tags$li(strong("50-75%:"), " Moderate risk - consider adjusting parameters"),
                     tags$li(strong("Below 50%:"), " High risk of running out of money")
                   )
                 )
             )
           )
    )
  )
}