# ============================================================================
# Credit Analytics - Server Logic
# ============================================================================



library(randomForest)
library(ggplot2)
library(reshape2)

insights_server <- function(input, output, session) {
  
  # Load credit data
  credit_data_path <- "data/insights/credit_score.csv"
  if (!file.exists(credit_data_path)) {
    # Try alternative path or create empty dataset
    credit_data <- data.frame()
    warning("Credit data file not found at: ", credit_data_path)
  } else {
    credit_data <- read.csv(credit_data_path)
  }
  
  # -------- PRETTY LABEL FUNCTION --------
  pretty_labels <- list(
    "INCOME" = "Income",
    "SAVINGS" = "Savings",
    "DEBT" = "Debt",
    "R_SAVINGS_INCOME" = "Savings-to-Income Ratio",
    "R_DEBT_INCOME" = "Debt-to-Income Ratio",
    "R_DEBT_SAVINGS" = "Debt-to-Savings Ratio",
    "CREDIT_SCORE" = "Credit Score"
  )
  
  format_label <- function(x) {
    if (x %in% names(pretty_labels)) return(pretty_labels[[x]])
    x <- gsub("_", " ", x)
    tools::toTitleCase(tolower(x))
  }
  
  # Convert categorical variables to factors (if columns exist)
  if (nrow(credit_data) > 0) {
    if ("CAT_GAMBLING" %in% names(credit_data)) credit_data$CAT_GAMBLING <- as.factor(credit_data$CAT_GAMBLING)
    if ("CAT_DEBT" %in% names(credit_data)) credit_data$CAT_DEBT <- as.factor(credit_data$CAT_DEBT)
    if ("CAT_CREDIT_CARD" %in% names(credit_data)) credit_data$CAT_CREDIT_CARD <- as.factor(credit_data$CAT_CREDIT_CARD)
    if ("CAT_MORTGAGE" %in% names(credit_data)) credit_data$CAT_MORTGAGE <- as.factor(credit_data$CAT_MORTGAGE)
    if ("CAT_SAVINGS_ACCOUNT" %in% names(credit_data)) credit_data$CAT_SAVINGS_ACCOUNT <- as.factor(credit_data$CAT_SAVINGS_ACCOUNT)
    if ("CAT_DEPENDENTS" %in% names(credit_data)) credit_data$CAT_DEPENDENTS <- as.factor(credit_data$CAT_DEPENDENTS)
  }
  
  # ===== FILTERED DATASET =====
  filtered_credit <- reactive({
    data <- credit_data
    if (input$default_filter == "Defaulted") data <- subset(data, DEFAULT == 1)
    if (input$default_filter == "Not Defaulted") data <- subset(data, DEFAULT == 0)
    data
  })
  
  # ===== SCATTER PLOT =====
  output$score_scatter_plot <- renderPlot({
    data <- filtered_credit()
    xvar <- input$x_var
    yvar <- input$y_var
    
    ggplot(data, aes_string(x = xvar, y = yvar)) +
      geom_point(color="#00808080", size=2) +
      geom_smooth(method="lm", color="#0099cc") +
      theme_minimal(base_size = 14) +
      scale_x_continuous(labels = scales::label_comma()) +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(
        x = format_label(xvar),
        y = format_label(yvar),
        title = paste("Relationship between", format_label(xvar), "and", format_label(yvar))
      )
  })
  
  
  # ===== HEATMAP =====
output$corr_heatmap <- renderPlot({

  # numeric-only columns
  numeric_cols <- credit_data[, sapply(credit_data, is.numeric)]

  corr_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")

  variances <- apply(numeric_cols, 2, var)
  top_vars <- names(sort(variances, decreasing = TRUE))[1:15]
  corr_small <- corr_matrix[top_vars, top_vars]

  # convert to long format
  corr_df <- reshape2::melt(corr_small)
  names(corr_df) <- c("Var1", "Var2", "Correlation")

  ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#4575b4",
      mid = "#ffffff",
      high = "#fdae61",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = "right"
    )
})


  
  # ===== FEATURE IMPORTANCE =====
  output$feature_importance <- renderPlot({
    model_data <- credit_data[, !(names(credit_data) %in% c("CUST_ID", "DEFAULT"))]
    
    rf <- randomForest(
      CREDIT_SCORE ~ ., 
      data = model_data,
      importance = TRUE,
      ntree = 300
    )
    
    df <- data.frame(
      Feature = rownames(importance(rf)),
      IncMSE = importance(rf)[, "%IncMSE"]
    )
    
    df <- head(df[order(df$IncMSE, decreasing=TRUE), ], 15)
    df$Pretty <- sapply(df$Feature, format_label)
    
    ggplot(df, aes(x = IncMSE, y = reorder(Pretty, IncMSE))) +
      geom_point(size=4, color="#1f77b4") +
      theme_minimal(base_size=14) +
      labs(
        title = "Top Predictors of Credit Score",
        x = "% Increase in MSE",
        y = "Variable"
      )
  })
  
  # ===== DOWNLOAD =====
  output$download_credit_data <- downloadHandler(
    filename = function() paste0("credit_score_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(credit_data, file, row.names = FALSE)
    }
  )
  
}
