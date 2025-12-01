# ============================================================================
# Credit Analytics - UI Component
# ============================================================================

insights_ui <- function() {
  tagList(
    
    div(
      style = "padding: 10px 0 25px 0;",
      h3("Interactive Credit Score Analytics",
         style = "font-weight:600; text-align:center;"),
      p("Explore how income, savings, debt, and spending patterns shape credit scores and default risk.",
        style = "text-align:center; font-size:16px; color:#555;"),
      
      fluidRow(
        # LEFT CONTROLS
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
                  "download_credit_data", "Download Dataset",
                  class = "btn-primary", style = "width:100%;"
                )
            )
          )
        ),
        
        # RIGHT VISUAL OUTPUT
        column(
          width = 8,
          
          card(
            card_header("Credit Score Relationship Explorer"),
            div(class = "p-3",
                plotOutput("score_scatter_plot", height = "450px"))
          ),
          
          card(
            card_header("Correlation Heatmap"),
            div(class = "p-3",
                p("Visualize relationships among key financial variables.",
                  style = "color:#666; margin-bottom:15px;"),
                plotOutput("corr_heatmap", height = "550px"))
          ),
          
          card(
            card_header("Feature Importance (Random Forest)"),
            div(class = "p-3",
                p("Which financial features matter most in predicting credit score?",
                  style = "color:#666; margin-bottom:15px;"),
                plotOutput("feature_importance", height = "550px"))
          )
        )
      )
    ),
    
    # ===== EXPLANATION SECTION =====
    div(
      class = "p-3",
      h4("How the Top Credit Score Predictors Are Calculated",
         style = "font-weight:600;"),
      
      p("The feature importance chart is generated using a Random Forest regression model trained to predict CREDIT_SCORE from all available numeric and categorical features in the dataset."),
      
      p(strong("Model Used:"), 
        " A Random Forest with 300 decision trees. Each tree makes a prediction, and the model averages their results to improve accuracy and stability."),
      
      p(strong("What Importance Means:"), 
        "The model measures how much each variable contributes to predicting credit score by tracking the increase in prediction error when that variable is randomly permuted."),
      
      p(strong("%IncMSE:"), 
        "This metric expresses how much the model's mean squared error worsens when a feature is disrupted. Larger values indicate stronger predictive power."),
      
      p(strong("Interpretation:"), 
        "Features with higher %IncMSE values are more influential in determining credit score.")
    ),
    
    # ===== VARIABLE DICTIONARY =====
    div(
      class = "p-3",
      h4("Variable Descriptions", style = "font-weight:600;"),
      
      p(strong("INCOME:"), "Represents the customer's total earnings over the past 12 months."),
      p(strong("SAVINGS:"), "Shows the amount the customer saved in the last 12 months."),
      p(strong("DEBT:"), "Indicates the customer's total outstanding debt."),
      p(strong("CREDIT_SCORE:"), "Measures overall credit quality, ranging from 300 to 850."),
      p(strong("DEFAULT:"), "Indicates whether the customer has defaulted on a loan (1 = yes, 0 = no)."),
      p(strong("R_SAVINGS_INCOME:"), "Shows what portion of income is saved annually."),
      p(strong("R_DEBT_INCOME:"), "Indicates how much of a customer's income is consumed by debt."),
      p(strong("R_DEBT_SAVINGS:"), "Compares total debt to total savings."),
      p(strong("T_{GROUP}_6:"), "Total spending in a category over the last 6 months."),
      p(strong("T_{GROUP}_12:"), "Total spending in a category over the last 12 months."),
      p(strong("R_{GROUP}:"), "Compares 6-month vs. 12-month spending in a category."),
      p(strong("R_{GROUP}_INCOME:"), "Income share spent annually in a given category."),
      p(strong("R_{GROUP}_SAVINGS:"), "Savings share spent annually in a given category."),
      p(strong("R_{GROUP}_DEBT:"), "Category spending relative to total debt.")
    )
  )

}
