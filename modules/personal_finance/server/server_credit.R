credit_server <- function(input, output, session) {
  
  library(randomForest)
  library(readxl)
  
  # Load and prepare data for credit card model
  clean_dataset <- read_excel("data/credit/clean_dataset.xlsx")
  
  # Remove ZipCode column if it exists
  if("ZipCode" %in% colnames(clean_dataset)) {
    clean_dataset$ZipCode <- NULL
  }
  
  # Make sure column names are correct (fix any spacing issues)
  colnames(clean_dataset) <- trimws(colnames(clean_dataset))
  
  # Convert categorical variables to factors
  clean_dataset$Approved <- as.factor(clean_dataset$Approved)
  clean_dataset$Industry <- as.factor(clean_dataset$Industry)
  clean_dataset$Ethnicity <- as.factor(clean_dataset$Ethnicity)
  clean_dataset$Citizen <- as.factor(clean_dataset$Citizen)
  
  # Train Random Forest model
  set.seed(123)
  rf_model <- randomForest(Approved ~ Gender + Age + Debt + Married + BankCustomer + 
                             Industry + Ethnicity + YearsEmployed + PriorDefault + 
                             Employed + CreditScore + DriversLicense + Citizen + 
                             Income, 
                           data = clean_dataset,
                           ntree = 500,
                           mtry = 4,
                           importance = TRUE)
  
  # Credit Card Prediction Logic
  prediction <- eventReactive(input$predict, {
    req(input$Gender, input$Age, input$Debt, input$Married, 
        input$BankCustomer, input$Industry, input$Ethnicity,
        input$YearsEmployed, input$PriorDefault, input$Employed,
        input$CreditScore, input$DriversLicense, input$Citizen,
        input$Income)
    
    new_data <- data.frame(
      Gender = as.numeric(input$Gender),
      Age = as.numeric(input$Age),
      Debt = as.numeric(input$Debt),
      Married = as.numeric(input$Married),
      BankCustomer = as.numeric(input$BankCustomer),
      Industry = factor(input$Industry, levels = levels(clean_dataset$Industry)),
      Ethnicity = factor(input$Ethnicity, levels = levels(clean_dataset$Ethnicity)),
      YearsEmployed = as.numeric(input$YearsEmployed),
      PriorDefault = as.numeric(input$PriorDefault),
      Employed = as.numeric(input$Employed),
      CreditScore = as.numeric(input$CreditScore),
      DriversLicense = as.numeric(input$DriversLicense),
      Citizen = factor(input$Citizen, levels = levels(clean_dataset$Citizen)),
      Income = as.numeric(input$Income)
    )
    
    probs <- predict(rf_model, newdata = new_data, type = "prob")
    pred_class <- predict(rf_model, newdata = new_data, type = "class")
    
    # Handle both numeric and character column names
    if("1" %in% colnames(probs)) {
      acceptance_prob <- probs[1, "1"]
      rejection_prob <- probs[1, "0"]
    } else {
      acceptance_prob <- probs[1, 2]
      rejection_prob <- probs[1, 1]
    }
    
    list(
      acceptance_prob = acceptance_prob,
      rejection_prob = rejection_prob,
      predicted_class = pred_class,
      data = new_data
    )
  })
  
  # Display prediction text
  output$prediction_text <- renderText({
    pred <- prediction()
    pct <- round(pred$acceptance_prob * 100, 1)
    
    if(pct >= 70) {
      paste0("High Probability of Acceptance: ", pct, "%")
    } else if(pct >= 40) {
      paste0("Moderate Probability of Acceptance: ", pct, "%")
    } else {
      paste0("Low Probability of Acceptance: ", pct, "%")
    }
  })
  
  # Create probability bar chart
  output$prob_plot <- renderPlot({
    pred <- prediction()
    
    probs <- c(pred$rejection_prob, pred$acceptance_prob)
    names <- c("Rejected", "Accepted")
    colors <- c("#ef4444", "#10b981")
    
    par(mar = c(4, 8, 2, 2))
    barplot(probs, 
            horiz = TRUE, 
            names.arg = names,
            col = colors,
            xlim = c(0, 1),
            xlab = "Probability",
            las = 1,
            main = "Acceptance vs Rejection Probability")
    abline(v = 0.5, lty = 2, col = "gray50")
  })
  
  # Display detailed probabilities
  output$prob_details <- renderText({
    pred <- prediction()
    paste0(
      "Probability of Acceptance: ", round(pred$acceptance_prob * 100, 2), "%\n",
      "Probability of Rejection: ", round(pred$rejection_prob * 100, 2), "%\n",
      "Predicted Class: ", ifelse(pred$predicted_class == "1", "APPROVED", "REJECTED")
    )
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    print(rf_model)
  })
  
  # Plot feature importance
  output$importance_plot <- renderPlot({
    imp <- importance(rf_model)
    imp_sorted <- imp[order(imp[, "MeanDecreaseGini"], decreasing = TRUE), ]
    
    par(mar = c(5, 10, 4, 2))
    barplot(imp_sorted[, "MeanDecreaseGini"],
            names.arg = rownames(imp_sorted),
            horiz = TRUE,
            las = 1,
            col = "#2563eb",
            xlab = "Mean Decrease in Gini",
            main = "Feature Importance in Random Forest Model")
  })
}