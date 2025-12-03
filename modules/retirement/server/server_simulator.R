# ============================================================================
# Monte Carlo Simulator - Server Logic (Auto-Update System)
# ============================================================================

# Monte Carlo simulation function (matches reference implementation)
sim <- function(startAmount = 100000, growthRate = 5.0, standardDev = 7.0, 
                inflation = 2.5, infStandardDev = 1.5, withdrawals = 1000, 
                years = 20, sims = 200, seed = NULL) {
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Convert percentages to decimals
  growthRate = growthRate / 100
  standardDev = standardDev / 100
  inflation = inflation / 100
  infStandardDev = infStandardDev / 100
  
  # Convert years to months
  years = 12 * years
  monthlyGR = growthRate / 12
  monthlySD = standardDev / sqrt(12)
  monthlyInfl = inflation / 12
  monthlyInfSD = infStandardDev / sqrt(12)
  
  # Generate random returns for investment and inflation
  monthlyRInvest = matrix(0, years, sims)
  monthlyRInf = matrix(0, years, sims)
  
  monthlyRInvest[] = rnorm(years * sims, mean = monthlyGR, sd = monthlySD)
  monthlyRInf[] = rnorm(years * sims, mean = monthlyInfl, sd = monthlyInfSD)
  
  # Simulate portfolio value over time
  nav = matrix(startAmount, years + 1, sims)
  for (j in 1:years) {
    nav[j + 1, ] = nav[j, ] * (1 + monthlyRInvest[j,] - monthlyRInf[j,]) - withdrawals
  }
  
  # Set negative values to zero (portfolio depleted)
  nav[nav < 0] = 0
  nav = nav / 1000000  # Convert to millions
  return(nav)
}

# Plotting function for simulation results
plot_nav <- function(nav) {
  # Set up side-by-side plots
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  # Left plot: Portfolio value trajectories
  matplot(nav,
          type = 'l', lwd = 0.5, lty = 1, col = 1:5,
          xlab = 'Months', ylab = 'Millions',
          main = 'Projected Value of Initial Capital')
  
  # Right plot: Success rate over time
  living = 1 - rowSums(nav == 0) / ncol(nav)
  
  plot(100 * living, las = 1, xlab = 'Months', ylab = 'Percentage Paying',
       main = 'Percentage of Paying Scenarios', ylim = c(0, 100))
  grid()
}

# Helper function to get parameters from input
getParameters <- function(prefix, input) {
  list(
    startAmount   = input[[paste0(prefix, "_startAmount")]],
    growthRate    = input[[paste0(prefix, "_growthRate")]],
    standardDev   = input[[paste0(prefix, "_standardDev")]],
    inflation     = input[[paste0(prefix, "_inflation")]],
    infStandardDev= input[[paste0(prefix, "_infStandardDev")]],
    withdrawals   = input[[paste0(prefix, "_withdrawals")]],
    years         = input[[paste0(prefix, "_years")]],
    sims          = 200
  )
}

simulator_server <- function(input, output, session) {
  
  # Reactive simulations for scenarios A and B with input validation
  # These now automatically re-run whenever any input changes
  navA <- reactive({
    params <- getParameters("case_a_for_retirement", input)
    
    # Validate inputs
    validate(
      need(params$startAmount > 0, "Starting amount must be positive"),
      need(params$withdrawals > 0, "Monthly withdrawals must be positive"),
      need(params$years > 0, "Years must be positive"),
      need(params$growthRate >= 0 && params$growthRate <= 30, "Growth rate must be between 0% and 30%"),
      need(params$inflation >= 0 && params$inflation <= 20, "Inflation must be between 0% and 20%")
    )
    
    params$seed <- 12345  # Fixed seed for reproducibility
    do.call(sim, params)
  })
  
  navB <- reactive({
    params <- getParameters("case_b_for_retirement", input)
    
    # Validate inputs
    validate(
      need(params$startAmount > 0, "Starting amount must be positive"),
      need(params$withdrawals > 0, "Monthly withdrawals must be positive"),
      need(params$years > 0, "Years must be positive"),
      need(params$growthRate >= 0 && params$growthRate <= 30, "Growth rate must be between 0% and 30%"),
      need(params$inflation >= 0 && params$inflation <= 20, "Inflation must be between 0% and 20%")
    )
    
    params$seed <- 12346  # Different seed for scenario B
    do.call(sim, params)
  })
  
  # Plot outputs
  output$a_distPlot <- renderPlot({
    req(navA())
    plot_nav(navA())
  })
  
  output$b_distPlot <- renderPlot({
    req(navB())
    plot_nav(navB())
  })
  
  # Summary text outputs
  output$a_summary <- renderText({
    nav <- navA()
    req(nav)
    
    # Calculate success rate (percentage of simulations that don't run out of money)
    final_values <- nav[nrow(nav), ]
    success_rate <- sum(final_values > 0) / length(final_values) * 100
    
    # Calculate median final value
    median_final <- median(final_values)
    
    # Build summary text
    paste0(
      "Success Rate: ", round(success_rate, 1), "%\n",
      "Median Final Value: $", format(round(median_final * 1000000), big.mark = ","), "\n",
      "Simulations: 200\n",
      "Duration: ", input$case_a_for_retirement_years, " years"
    )
  })
  
  output$b_summary <- renderText({
    nav <- navB()
    req(nav)
    
    # Calculate success rate
    final_values <- nav[nrow(nav), ]
    success_rate <- sum(final_values > 0) / length(final_values) * 100
    
    # Calculate median final value
    median_final <- median(final_values)
    
    # Build summary text
    paste0(
      "Success Rate: ", round(success_rate, 1), "%\n",
      "Median Final Value: $", format(round(median_final * 1000000), big.mark = ","), "\n",
      "Simulations: 200\n",
      "Duration: ", input$case_b_for_retirement_years, " years"
    )
  })
}