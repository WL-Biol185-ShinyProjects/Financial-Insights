# About Page UI
about_ui <- function() {
  fluidRow(
    column(
      12,
      card(
        card_header("About"),
        div(
          class = "p-4",
          h3("Financial Data Analysis and Planning Dashboard", class = "mb-3"),
          p(
            style = "font-size: 16px; line-height: 1.6;",
            "An integrated platform for global economic analysis and personal financial planning. ",
            "This application connects macroeconomic trends with individual financial decision-making through ",
            "interactive visualizations, statistical analysis, and predictive modeling."
          ),
          hr(),
          h4("Project Description", class = "mt-4"),
          p(
            style = "font-size: 15px; line-height: 1.6;",
            "The dashboard consists of three interconnected modules: a Global Macroeconomic Explorer analyzing ",
            "economic indicators across 200+ countries and U.S. states, Personal Finance Tools providing savings ",
            "projections and loan approval probability estimates, and a Retirement Risk Simulator using Monte Carlo ",
            "methods to assess long-term financial outcomes."
          ),
          hr(),
          h4("Data Sources", class = "mt-4"),
          p(
            style = "font-size: 15px; line-height: 1.6;",
            strong("World Bank World Development Indicators (WDI):"), " Global macroeconomic data covering GDP, ",
            "inflation, unemployment, government debt, and other indicators for 200+ countries from 1960-2023. ",
            "Data accessed via the WDI R package API. License: Creative Commons Attribution 4.0 (CC-BY 4.0). ",
            tags$a(
              href = "https://datatopics.worldbank.org/world-development-indicators/", target = "_blank",
              "https://datatopics.worldbank.org/world-development-indicators/"
            ),
            tags$br(), tags$br(),
            strong("U.S. State-Level Data:"), " Demographic and economic indicators including median household income, ",
            "unemployment rates, poverty rates, and cost of living indices for all 50 U.S. states. ",
            "Sources: U.S. Census Bureau American Community Survey (ACS) 2023 5-Year Estimates (Tables B19013, B17001), ",
            "Bureau of Labor Statistics Local Area Unemployment Statistics (LAUS) and Job Openings and Labor Turnover Survey (JOLTS), ",
            "and Missouri Economic Research and Information Center (MERIC) Cost of Living Data Series. License: Public Domain (U.S. Government Work). ",
            tags$a(href = "https://www.census.gov/programs-surveys/acs", target = "_blank", "Census ACS"), ", ",
            tags$a(href = "https://www.bls.gov/lau/", target = "_blank", "BLS LAUS"), ", ",
            tags$a(href = "https://www.bls.gov/jlt/", target = "_blank", "BLS JOLTS"), ", ",
            tags$a(href = "https://meric.mo.gov/data/cost-living-data-series", target = "_blank", "MERIC"),
            tags$br(), tags$br(),
            strong("Commodity Prices:"), " Gold prices (USD per troy ounce) from DataHub.io Core Gold Prices Dataset ",
            "and WTI crude oil prices (USD per barrel) from FRED Economic Data. Both datasets are publicly available ",
            "and used for comparing U.S. macroeconomic indicators with commodity market trends. ",
            tags$a(href = "https://datahub.io/core/gold-prices", target = "_blank", "DataHub.io Gold Prices"), ", ",
            tags$a(href = "https://fred.stlouisfed.org/series/DCOILWTICO", target = "_blank", "FRED Oil Prices"),
            tags$br(), tags$br(),
            strong("Personal Finance Training Datasets:"), tags$br(),
            "Loan Approval Dataset: Historical loan application records used to train a gradient descent ",
            "logistic regression model for predicting loan approval probability based on borrower characteristics. ",
            "Note: Dataset source and license information should be verified for academic use.", tags$br(), tags$br(),
            "Credit Card Approval Dataset: Historical credit card application records used to train a Random Forest ",
            "classification model (500 decision trees) for predicting approval probability. ",
            "Note: Dataset source and license information should be verified for academic use.", tags$br(), tags$br(),
            "Credit Score Analytics Dataset: Customer financial data including income, savings, debt, spending patterns, ",
            "and credit scores. Used for Random Forest regression analysis (300 trees) to identify key credit score predictors. ",
            "Note: Dataset source and license information should be verified for academic use."
          ),
          hr(),
          h4("Methodology", class = "mt-4"),
          p(
            style = "font-size: 15px; line-height: 1.6;",
            strong("Macroeconomic Analysis:"), " Employs advanced statistical methods including ANOVA for regional comparisons, ",
            "multiple linear regression for determinant analysis (e.g., predicting life expectancy from economic factors), ",
            "and Chi-Square test of independence to examine associations between economic indicator categories and geographic regions. ",
            "Also includes Pearson correlation matrices, descriptive statistics, and interactive visualizations.",
            tags$br(), tags$br(),
            strong("Personal Finance Tools:"), " Use standard compound interest formulas for savings projections and ",
            "amortization calculations for loan analysis. Loan approval predictions utilize a custom gradient descent ",
            "logistic regression model trained on historical loan data, providing probability scores based on income, ",
            "credit score, loan amount, and employment history. Credit card approval predictions use a Random Forest ",
            "machine learning model with 500 decision trees. Credit score analytics employ Random Forest regression ",
            "with 300 trees to identify key financial predictors through feature importance analysis.",
            tags$br(), tags$br(),
            strong("Retirement Simulations:"), " Implement Monte Carlo methods with monthly granularity, modeling ",
            "investment returns and inflation as normally distributed random variables. Each simulation runs 200 scenarios ",
            "to calculate success rates and portfolio trajectory distributions with 80% and 95% confidence intervals."
          ),
          hr(),
          h4("Key Findings and Research Questions", class = "mt-4"),
          p(
            style = "font-size: 15px; line-height: 1.6;",
            "The application addresses questions such as: Are there statistically significant differences in economic ",
            "indicators across global regions (ANOVA)? How much of the variation in life expectancy can be explained by ",
            "economic factors like GDP, unemployment, and government debt (Multiple Regression)? Are economic indicator ",
            "categories independent of geographic regions (Chi-Square Test)? What savings rates and investment strategies are needed ",
            "to achieve retirement goals? The interactive tools enable users to explore these questions through advanced ",
            "statistical analysis, hypothesis testing, and predictive modeling."
          ),
          hr(),
          h4("Development Team", class = "mt-4"),
          p(
            style = "font-size: 15px;",
            strong("Course:"), " BIOL 185 - Data Science: Visualizing and Exploring Big Data", tags$br(),
            strong("Semester:"), " Fall 2025", tags$br(), tags$br(),
            strong("Team Members:"), tags$br(),
            "• Ammar Alhajmee - Global Macroeconomic Explorer Module", tags$br(),
            "• Bemnet Ali - Personal Finance Tools Module", tags$br(),
            "• Colin Bridges - Retirement Risk Simulator Module"
          ),
          hr(),
          h4("License", class = "mt-4"),
          p(
            style = "font-size: 15px;",
            "This project is provided for educational and research purposes. Source code and documentation are ",
            "available for academic use."
          ),
          hr(),
          div(
            class = "alert alert-warning mt-3",
            h5("Disclaimer"),
            p(
              class = "mb-2", style = "font-size: 14px;",
              strong("Educational Purpose:"), " This application is intended for educational and informational ",
              "purposes only. It does not constitute financial, investment, or legal advice."
            ),
            p(
              class = "mb-2", style = "font-size: 14px;",
              strong("No Financial Advice:"), " Users should consult qualified financial professionals before making ",
              "investment or financial planning decisions. Historical data and simulations do not guarantee future performance."
            ),
            p(
              class = "mb-0", style = "font-size: 14px;",
              strong("AI-Assisted Development:"), " This application was developed with assistance from various ",
              "artificial intelligence models. Code generation, documentation, and implementation were facilitated through ",
              "AI-powered development tools."
            )
          )
        )
      )
    )
  )
}
