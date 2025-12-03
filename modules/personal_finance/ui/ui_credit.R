credit_ui <- function() {
  tagList(
    tags$div(
      style = "padding: 24px;",
      
      # Header
      tags$div(
        class = "card",
        tags$div(
          class = "card-header",
          tags$h4(bs_icon("credit-card"), " Credit Card Application Predictor", style = "margin: 0;")
        ),
        tags$div(
          class = "card-body",
          tags$p("Use our Random Forest machine learning model to predict credit card approval probability based on your financial profile.")
        )
      ),
      
      # Main content
      fluidRow(
        # Input column
        column(4,
               tags$div(
                 class = "card",
                 tags$div(
                   class = "card-header",
                   "Application Details"
                 ),
                 tags$div(
                   class = "card-body",
                   
                   selectInput("Gender", 
                               "Gender:",
                               choices = c("Select..." = "", "Male" = "1", "Female" = "0"),
                               selected = "1"),
                   
                   sliderInput("Age", 
                               "Age:",
                               value = 35,
                               min = 18,
                               max = 100,
                               step = 1),
                   
                   sliderInput("Debt", 
                               "Total Debt ($):",
                               value = 5000,
                               min = 0,
                               max = 50000,
                               step = 100),
                   
                   selectInput("Married", 
                               "Marital Status:",
                               choices = c("Select..." = "", "Married" = "1", "Not Married" = "0"),
                               selected = "0"),
                   
                   selectInput("BankCustomer", 
                               "Existing Bank Customer?:",
                               choices = c("Select..." = "", "Yes" = "1", "No" = "0"),
                               selected = "1"),
                   
                   selectInput("Industry", 
                               "Industry:",
                               choices = c("Select..." = "", "CommunicationServices", "ConsumerDiscretionary", "ConsumerStaples", 
                                           "Education", "Energy", "Financials", "Healthcare", 
                                           "Industrials", "InformationTechnology", "Materials", 
                                           "Real Estate", "Research", "Transport", "Utilities"),
                               selected = "Financials"),
                   
                   selectInput("Ethnicity", 
                               "Ethnicity:",
                               choices = c("Select..." = "", "White", "Latino", "Black", "Asian", "Other"),
                               selected = "White"),
                   
                   sliderInput("YearsEmployed", 
                               "Years Employed:",
                               value = 5,
                               min = 0,
                               max = 40,
                               step = 0.5),
                   
                   selectInput("PriorDefault", 
                               "No Prior Defaults?:",
                               choices = c("Select..." = "", "Yes" = "1", "No" = "0"),
                               selected = "1"),
                   
                   selectInput("Employed", 
                               "Currently Employed?:",
                               choices = c("Select..." = "", "Yes" = "1", "No" = "0"),
                               selected = "1"),
                   
                   sliderInput("CreditScore", 
                               "Credit Score:",
                               value = 650,
                               min = 300,
                               max = 850,
                               step = 5),
                   
                   selectInput("DriversLicense", 
                               "Driver's License?:",
                               choices = c("Select..." = "", "Yes" = "1", "No" = "0"),
                               selected = "1"),
                   
                   selectInput("Citizen", 
                               "Citizenship Status:",
                               choices = c("Select..." = "", "ByBirth", "ByOtherMeans", "Temporary"),
                               selected = "ByBirth"),
                   
                   sliderInput("Income", 
                               "Annual Income ($):",
                               value = 50000,
                               min = 0,
                               max = 200000,
                               step = 1000),
                   
                   br(),
                   actionButton("predict", "Calculate Approval Probability", 
                                class = "btn btn-primary", 
                                style = "width: 100%;")
                 )
               )
        ),
        
        # Results column
        column(8,
               # Prediction Results
               conditionalPanel(
                 condition = "input.predict > 0",
                 tags$div(
                   class = "card",
                   tags$div(
                     class = "card-header",
                     textOutput("prediction_text")
                   ),
                   tags$div(
                     class = "card-body",
                     plotOutput("prob_plot", height = "200px"),
                     tags$hr(),
                     verbatimTextOutput("prob_details")
                   )
                 )
               ),
               
               # Instructions
               tags$div(
                 class = "card",
                 tags$div(
                   class = "card-header",
                   tags$h5(bs_icon("lightbulb"), " How to Use", style = "margin: 0;")
                 ),
                 tags$div(
                   class = "card-body",
                   tags$ol(
                     tags$li("Fill out all application fields in the left panel"),
                     tags$li("Click 'Calculate Approval Probability' to see your results"),
                     tags$li("The prediction is based on a Random Forest model trained on historical data"),
                     tags$li("View the Model Information tab below for technical details")
                   )
                 )
               ),
               
               # Model Information
               tags$div(
                 class = "card",
                 tags$div(
                   class = "card-header",
                   tags$h5(bs_icon("gear"), " Model Information", style = "margin: 0;")
                 ),
                 tags$div(
                   class = "card-body",
                   tags$h6("Feature Importance"),
                   plotOutput("importance_plot", height = "400px"),
                   tags$p(class = "text-muted", "This chart shows which features have the most impact on approval decisions.")
                 )
               )
        )
      )
    )
  )
}