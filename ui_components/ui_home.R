# Home Page UI
home_ui <- function() {
  fluidRow(
    # Hero Section
    column(
      12,
      card(
        card_header(tags$span(bs_icon("graph-up-arrow"), " Financial Insight")),
        div(
          class = "p-4",
          h2("Financial Data Analysis and Planning Dashboard", class = "display-6 fw-bold"),
          p(
            class = "lead text-muted",
            "A comprehensive platform integrating global macroeconomic analysis, personal finance tools, and retirement planning simulations."
          ),
          hr(),
          p(
            style = "font-size: 1.1rem; line-height: 1.7;",
            "This application provides three interconnected analytical modules: ",
            strong("Global Macroeconomic Explorer"), " for analyzing economic indicators across 200+ countries using advanced statistical methods; ",
            strong("Personal Finance Tools"), " for savings projections, loan analysis, and financial planning guidance; and ",
            strong("Retirement Risk Simulator"), " for Monte Carlo-based long-term portfolio simulations. ",
            "Designed for educational and research purposes, the dashboard enables users to explore relationships between global economic trends and individual financial decision-making."
          )
        )
      )
    ),

    # Module Cards - Streamlined
    column(
      4,
      card(
        card_header(tags$span(bs_icon("globe"), " Macro Economy")),
        div(
          style = "min-height: 180px;",
          p("Analyze global trends across 200+ countries with advanced statistical methods."),
          tags$ul(
            class = "list-unstyled",
            tags$li(bs_icon("check-circle"), " Time Series & Correlations"),
            tags$li(bs_icon("check-circle"), " Statistical Analysis (ANOVA, Regression, Chi-Square)"),
            tags$li(bs_icon("check-circle"), " Interactive Maps & Regional Trends"),
            tags$li(bs_icon("check-circle"), " Commodity Price Comparisons")
          ),
          actionButton("btn_goto_macro", "Explore Macro Data",
            icon = icon("arrow-right"),
            class = "btn-primary w-100 mt-3"
          )
        )
      )
    ),
    column(
      4,
      card(
        card_header(tags$span(bs_icon("wallet2"), " Personal Finance")),
        div(
          style = "min-height: 180px;",
          p("Tools to manage your wealth today."),
          tags$ul(
            class = "list-unstyled",
            tags$li(bs_icon("check-circle"), " Credit Score Analytics"),
            tags$li(bs_icon("check-circle"), " Savings Growth Projector"),
            tags$li(bs_icon("check-circle"), " Loan Amortization & Approval"),
            tags$li(bs_icon("check-circle"), " Credit Card Predictor"),
            tags$li(bs_icon("check-circle"), " 50/30/20 Financial Guide")
          ),
          actionButton("btn_goto_finance", "Plan Your Finances",
            icon = icon("arrow-right"),
            class = "btn-primary w-100 mt-3"
          )
        )
      )
    ),
    column(
      4,
      card(
        card_header(tags$span(bs_icon("piggy-bank"), " Retirement")),
        div(
          style = "min-height: 180px;",
          p("Simulate your long-term financial future."),
          tags$ul(
            class = "list-unstyled",
            tags$li(bs_icon("check-circle"), " Monte Carlo Simulations"),
            tags$li(bs_icon("check-circle"), " Risk Scenario Analysis"),
            tags$li(bs_icon("check-circle"), " Portfolio Probability")
          ),
          actionButton("btn_goto_retirement", "Run Simulations",
            icon = icon("arrow-right"),
            class = "btn-primary w-100 mt-3"
          )
        )
      )
    )
  )
}
