# ============================================================================
# Loan Calculator - Server Logic
# ============================================================================

loans_server <- function(input, output, session) {
  
  # Load loan approval training data
  loan_approval_path <- "data/loan/loan_approval.xlsx"
  
  if (!file.exists(loan_approval_path)) {
    stop("Loan approval data not found! Please ensure data/loan/loan_approval.xlsx exists.")
  }
  
  loan_approval <- read_excel(loan_approval_path)
  loan_clean <- loan_approval[, -c(1, 2, 7)]  # Remove name, city, points columns
  loan_clean$loan_approved <- as.numeric(loan_clean$loan_approved)
  
  # Train model function using gradient descent
  train_model <- function(data) {
    # Extract features (X) and labels (y)
    X <- as.matrix(data[, c("income", "credit_score", "loan_amount", "years_employed")])
    y <- data$loan_approved
    
    # Normalize features (important for gradient descent)
    means <- colMeans(X)
    stds <- apply(X, 2, sd)
    X_norm <- scale(X, center = means, scale = stds)
    
    # Gradient descent to find weights
    weights <- rep(0, 5)  # bias + 4 features
    learning_rate <- 0.1
    iterations <- 1000
    
    for (iter in 1:iterations) {
      gradients <- rep(0, 5)
      
      for (i in seq_len(nrow(X_norm))) {
        features <- c(1, X_norm[i, ])
        z <- sum(features * weights)
        prediction <- 1 / (1 + exp(-z))  # Sigmoid function
        error <- prediction - y[i]
        
        gradients <- gradients + error * features
      }
      
      weights <- weights - (learning_rate / nrow(X_norm)) * gradients
    }
    
    return(list(weights = weights, means = means, stds = stds))
  }
  
  # Predict probability function
  predict_approval <- function(model, income, credit_score, loan_amount, years_employed) {
    # Normalize the input using training data statistics
    features <- c(income, credit_score, loan_amount, years_employed)
    # Handle zero standard deviation (constant features in training)
    normalized <- ifelse(model$stds == 0, 0, (features - model$means) / model$stds)
    
    # Calculate probability using trained weights
    z <- model$weights[1] + sum(normalized * model$weights[2:5])
    probability <- 1 / (1 + exp(-z))  # Sigmoid function
    
    return(probability)
  }
  
  # Train the model once (could be cached, but training is fast)
  trained_model <- train_model(loan_clean)
  
  # Reactive loan calculation - Auto-updates on input change
  loan_data <- reactive({
    req(input$loan_amount, input$loan_income, input$loan_score, input$loan_years_employed,
        input$loan_interest_rate, input$loan_term_years)
    
    P <- input$loan_amount
    r <- input$loan_interest_rate / 100 / 12  # Monthly interest rate
    n <- input$loan_term_years * 12  # Total number of payments
    
    # Calculate monthly payment using amortization formula
    if (r > 0) {
      monthly_payment <- P * (r * (1 + r)^n) / ((1 + r)^n - 1)
    } else {
      monthly_payment <- P / n
    }
    
    # Calculate total payments and interest
    total_payments <- monthly_payment * n
    total_interest <- total_payments - P
    
    # Generate amortization schedule
    balance <- P
    amortization_schedule <- data.frame(
      Month = integer(),
      Payment = numeric(),
      Principal = numeric(),
      Interest = numeric(),
      Remaining_Balance = numeric()
    )
    
    for (month in 1:n) {
      interest_payment <- balance * r
      principal_payment <- monthly_payment - interest_payment
      balance <- balance - principal_payment
      
      if (balance < 0) balance <- 0
      
      amortization_schedule <- rbind(amortization_schedule, data.frame(
        Month = month,
        Payment = round(monthly_payment, 2),
        Principal = round(principal_payment, 2),
        Interest = round(interest_payment, 2),
        Remaining_Balance = round(balance, 2)
      ))
    }
    
    # Predict approval probability using trained gradient descent model
    prob <- predict_approval(
      trained_model,
      input$loan_income,
      input$loan_score,
      P,
      input$loan_years_employed
    )
    
    list(
      prob = prob,
      monthly_payment = monthly_payment,
      total_payments = total_payments,
      total_interest = total_interest,
      principal_paid = P,
      interest_paid = total_interest,
      remaining_balance = balance,
      amortization_schedule = amortization_schedule
    )
  })
  
  output$loan_summary <- renderUI({
    res <- loan_data()
    req(res)
    
    prob_pct <- round(res$prob * 100, 1)
    
    color <- if(prob_pct >= 70) "success" else if(prob_pct >= 40) "warning" else "danger"
    icon_name <- if(prob_pct >= 70) "check-circle" else if(prob_pct >= 40) "exclamation-circle" else "x-circle"
    
    approval_text <- if(prob_pct >= 70) {
      "Strong likelihood of approval! Your financial profile looks good."
    } else if(prob_pct >= 40) {
      "Moderate approval chances. Consider the recommendations below to improve your odds."
    } else {
      "Lower approval probability. Focus on improving key factors before applying."
    }
    
    tagList(
      div(class = paste0("alert alert-", color),
          h5(bs_icon(icon_name), " ", paste0(prob_pct, "% Approval Probability")),
          p(class = "mb-0", approval_text)
      ),
      div(class = "card mt-3",
          div(class = "card-body",
              h5("Loan Payment Summary", class = "card-title"),
              p(strong("Monthly Payment:"), paste0("$", format(round(res$monthly_payment, 2), big.mark = ","))),
              p(strong("Total Payments:"), paste0("$", format(round(res$total_payments, 2), big.mark = ","))),
              p(strong("Total Interest:"), paste0("$", format(round(res$total_interest, 2), big.mark = ","))),
              p(strong("Principal Amount:"), paste0("$", format(round(res$principal_paid, 2), big.mark = ",")))
          )
      ),
      uiOutput("loan_recommendations")
    )
  })
  
  output$loan_recommendations <- renderUI({
    recommendations <- character(0)
    
    if(input$loan_score < 700) {
      recommendations <- c(recommendations, 
                           "Improve your credit score by paying bills on time and reducing credit card balances")
    }
    
    if(input$loan_years_employed < 2) {
      recommendations <- c(recommendations,
                           "Build more employment history before applying for larger loans")
    }
    
    if(input$loan_amount > input$loan_income * 0.5) {
      recommendations <- c(recommendations,
                           "Your loan amount is high relative to your income. Consider saving for a larger down payment")
    }
    
    if(length(recommendations) == 0) {
      recommendations <- "Your financial profile is strong! You're in good shape to apply."
    }
    
    div(class = "alert alert-info",
        h5("Recommendations"),
        tags$ul(
          lapply(recommendations, function(rec) tags$li(rec))
        )
    )
  })
  
  # Amortization Plot
  output$loan_plot <- renderPlotly({
    loan_info <- loan_data()
    req(loan_info$amortization_schedule)
    
    schedule <- loan_info$amortization_schedule
    
    plot_ly(schedule, x = ~Month) %>%
      add_trace(y = ~Principal, name = "Principal", type = "scatter", mode = "lines", 
                line = list(color = "#10b981", width = 2)) %>%
      add_trace(y = ~Interest, name = "Interest", type = "scatter", mode = "lines",
                line = list(color = "#ef4444", width = 2)) %>%
      layout(
        title = list(text = "Principal vs Interest Over Time", font = list(color = "#1e293b", size = 18)),
        xaxis = list(title = "Month", gridcolor = "#e2e8f0"),
        yaxis = list(title = "Amount ($)", gridcolor = "#e2e8f0"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Amortization Schedule Table
  output$loan_amort_table <- renderDT({
    loan_info <- loan_data()
    req(loan_info$amortization_schedule)
    
    schedule <- loan_info$amortization_schedule
    colnames(schedule) <- c("Month", "Payment ($)", "Principal ($)", "Interest ($)", "Remaining Balance ($)")
    
    datatable(
      schedule,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(columns = c("Payment ($)", "Principal ($)", "Interest ($)", "Remaining Balance ($)"), 
                     currency = "$", digits = 2)
  })
  
}