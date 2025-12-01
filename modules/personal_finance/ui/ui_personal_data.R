# ============================================================================
# Data Insights - UI Component
# ============================================================================
personal_data_ui <- function() {
  fluidRow(
    column(
      12,
      card(
        card_header("Dataset Explorer"),
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          p("Browse, search, and download personal finance datasets used in this application.", class = "mb-0"),
          downloadButton("download_csv", "Download Full CSV", class = "btn-primary btn-sm")
        ),
        hr(),
        div(
          class = "alert alert-light border",
          h6(bs_icon("database"), " Dataset Selection", class = "mb-2"),
          selectizeInput("dataset_selector", "Select Dataset:",
                         choices = c(
                           "Loan Approval Dataset" = "loan",
                           "Credit Card Approval Dataset" = "credit"
                         ),
                         selected = "loan",
                         options = list(dropdownParent = "body")
          ),
          p(
            class = "mb-1 small mt-2",
            conditionalPanel(
              condition = "input.dataset_selector == 'loan'",
              strong("Loan Approval Dataset"), tags$br(),
              "This dataset contains information about loan applications and their approval status. ",
              "It includes various financial factors that influence whether a person was accepted for a loan, ",
              "such as applicant income, credit history, loan amount requested, property information, and final approval decision."
            ),
            conditionalPanel(
              condition = "input.dataset_selector == 'credit'",
              strong("Credit Card Approval Dataset"), tags$br(),
              "This dataset measures whether individuals were approved for credit cards based on their financial profiles. ",
              "Key factors include personal financial information, credit score, existing debt, employment status, and final approval decision."
            )
          )
        )
      )
    ),
    column(
      12,
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span("Dataset Statistics"),
          tags$small(class = "text-muted", "Quick overview of the selected dataset")
        ),
        card_body(
          fluidRow(
            column(3, uiOutput("stat_rows")),
            column(3, uiOutput("stat_columns")),
            column(3, uiOutput("stat_numeric")),
            column(3, uiOutput("stat_categorical"))
          )
        )
      )
    ),
    column(
      12,
      card(
        card_header("Raw Data"),
        div(
          style = "overflow-x: auto; max-width: 100%;",
          DTOutput("data_table")
        )
      )
    )
  )
}