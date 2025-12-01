# ============================================================================
# Credit Analytics - UI Component
# ============================================================================

library(ggplot2)


insights_ui <- function() {
  div(
    style = "padding: 10px 0 25px 0;",
    h3("Interactive Credit Score Analytics",
       style = "font-weight:600; text-align:center;"),
    p("Explore how income, savings, debt, and spending patterns shape credit scores and default risk.",
      style = "text-align:center; font-size:16px; color:#555;"),
    
    fluidRow(
      # =============================
      # LEFT CONTROLS PANEL
      # =============================
      column(
        width = 4,
        card(
          card_header("Controls"),
          div(class = "p-3",
            selectInput(
              "default_filter", "Filter by Default Status:",
              choices = c("All", "Defaulted", "Not Defaulted"),
              selected = "All"
            ),
            
            selectInput(
              "x_var", "Select X-Axis Variable:",
              choices = c(
                "Income" = "INCOME",
                "Savings" = "SAVINGS",
                "Debt" = "DEBT",
                "Savings-to-Income Ratio" = "R_SAVINGS_INCOME",
                "Debt-to-Income Ratio" = "R_DEBT_INCOME",
                "Debt-to-Savings Ratio" = "R_DEBT_SAVINGS",
                "Credit Score" = "CREDIT_SCORE"
              ),
              selected = "INCOME"
            ),
            
            selectInput(
              "y_var", "Select Y-Axis Variable:",
              choices = c(
                "Income" = "INCOME",
                "Savings" = "SAVINGS",
                "Debt" = "DEBT",
                "Savings-to-Income Ratio" = "R_SAVINGS_INCOME",
                "Debt-to-Income Ratio" = "R_DEBT_INCOME",
                "Debt-to-Savings Ratio" = "R_DEBT_SAVINGS",
                "Credit Score" = "CREDIT_SCORE"
              ),
              selected = "CREDIT_SCORE"
            ),
            
            br(),
            downloadButton(
              "download_credit_data",
              "Download Dataset",
              class = "btn-primary",
              style = "width:100%;"
            )
          )
        )
      ),
      
      # =============================
      # RIGHT VISUAL OUTPUT PANEL
      # =============================
      column(
        width = 8,
        card(
          card_header("Credit Score Relationship Explorer"),
          div(class = "p-3",
            plotOutput("score_scatter_plot", height = "450px")
          )
        ),
        
        card(
          card_header("Correlation Heatmap"),
          div(class = "p-3",
            p("Visualize relationships among key financial variables.",
              style = "color:#666; margin-bottom:15px;"),
            plotOutput("corr_heatmap", height = "550px")
          )
        ),
        
        card(
          card_header("Feature Importance (Random Forest)"),
          div(class = "p-3",
            p("Which financial features matter most in predicting credit score?",
              style = "color:#666; margin-bottom:15px;"),
            plotOutput("feature_importance", height = "550px")
          )
        )
      )
    )
  )
}
