# ============================================================================
# Personal Finance Data Explorer - Server Component
# ============================================================================

personal_data_server <- function(input, output, session) {
  
  # Load loan dataset (loan_approval.xlsx)
  loan_data <- reactive({
    file_path <- "data/loan/loan_approval.xlsx"
    
    if (!file.exists(file_path)) {
      showNotification(
        paste("Loan dataset not found at:", file_path), 
        type = "error", 
        duration = 5
      )
      return(NULL)
    }
    
    tryCatch({
      df <- readxl::read_excel(file_path)
      df
    }, error = function(e) {
      showNotification(
        paste("Error reading loan data:", e$message), 
        type = "error", 
        duration = 5
      )
      return(NULL)
    })
  })
  
  # Load credit dataset (cleaned_dataset.xlsx)
  credit_data <- reactive({
    file_path <- "data/credit/clean_dataset.xlsx"
    
    if (!file.exists(file_path)) {
      showNotification(
        paste("Credit card dataset not found at:", file_path), 
        type = "error", 
        duration = 5
      )
      return(NULL)
    }
    
    tryCatch({
      df <- readxl::read_excel(file_path)
      df
    }, error = function(e) {
      showNotification(
        paste("Error reading credit data:", e$message), 
        type = "error", 
        duration = 5
      )
      return(NULL)
    })
  })
  
  # Get selected dataset
  selected_data <- reactive({
    req(input$dataset_selector)
    
    data <- if (input$dataset_selector == "loan") {
      loan_data()
    } else if (input$dataset_selector == "credit") {
      credit_data()
    } else {
      NULL
    }
    
    req(data)
    return(data)
  })
  
  # ---- Dataset Statistics ----
  
  output$stat_rows <- renderUI({
    data <- selected_data()
    if (is.null(data)) return(div(class = "alert alert-warning", "No data loaded"))
    
    div(
      class = "text-center p-3 bg-primary bg-opacity-10 rounded",
      div(class = "fs-3 fw-bold text-primary", format(nrow(data), big.mark = ",")),
      div(class = "small text-muted", bs_icon("list-ol"), " Total Rows")
    )
  })
  
  output$stat_columns <- renderUI({
    data <- selected_data()
    if (is.null(data)) return(NULL)
    
    div(
      class = "text-center p-3 bg-info bg-opacity-10 rounded",
      div(class = "fs-3 fw-bold text-info", ncol(data)),
      div(class = "small text-muted", bs_icon("columns"), " Total Columns")
    )
  })
  
  output$stat_numeric <- renderUI({
    data <- selected_data()
    if (is.null(data)) return(NULL)
    
    num_count <- sum(sapply(data, is.numeric))
    
    div(
      class = "text-center p-3 bg-success bg-opacity-10 rounded",
      div(class = "fs-3 fw-bold text-success", num_count),
      div(class = "small text-muted", bs_icon("123"), " Numeric Columns")
    )
  })
  
  output$stat_categorical <- renderUI({
    data <- selected_data()
    if (is.null(data)) return(NULL)
    
    cat_count <- sum(sapply(data, function(x) is.character(x) || is.factor(x)))
    
    div(
      class = "text-center p-3 bg-warning bg-opacity-10 rounded",
      div(class = "fs-3 fw-bold text-warning", cat_count),
      div(class = "small text-muted", bs_icon("tags"), " Categorical Columns")
    )
  })
  
  # ---- Data Table ----
  output$data_table <- renderDT({
    data <- selected_data()
    if (is.null(data)) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'lfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = "display nowrap compact stripe hover",
      filter = "top",
      rownames = FALSE
    )
  })
  
  # ---- Download Handler ----
  output$download_csv <- downloadHandler(
    filename = function() {
      dataset_name <- if (input$dataset_selector == "loan") {
        "loan_approval"
      } else {
        "credit_card_approval"
      }
      paste0(dataset_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- selected_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}


