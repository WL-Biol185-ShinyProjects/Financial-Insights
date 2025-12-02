# Financial Data Analysis and Planning Dashboard

**BIOL 185 Group Project - Fall 2025**

A modular Shiny web application integrating global macroeconomic analysis, personal finance planning, and retirement risk simulation to explore how global economic conditions influence individual financial decisions and long-term retirement outcomes.

---

## Project Description

This dashboard integrates three analytical layers:

- **Global Macroeconomic Explorer**: Analysis of economic indicators across 200+ countries from 1960-2023
- **Personal Finance Tools**: Savings calculators, loan analysis, and budget planning guides
- **Retirement Risk Simulator**: Monte Carlo simulations for long-term retirement planning scenarios

The application uses classical statistical methods, interactive visualizations, and computational analysis to provide insights into economic trends and personal financial planning.

---

## Team Members

**Course:** BIOL 185 - Data Science: Visualizing and Exploring Big Data
**Semester:** Fall 2025

- **[Ammar Alhajmee]** - Global Macroeconomic Explorer Module
- **[Bemnet Ali]** - Personal Finance Tools Module
- **[Colin Bridges]** - Retirement Risk Simulator Module

---

## Modules and Features

### Module 1: Global Macroeconomic Explorer

**Features:**
- Time-series analysis of economic indicators across 200+ countries
- Dual-axis indicator relationships for country-specific analysis
- Indicator vs. commodity prices (gold and oil) comparison - U.S. specific
- Correlation matrix with country filtering
- Interactive world maps with dynamic color scaling and multi-indicator comparison (add multiple indicators to compare side-by-side)
- Regional trend comparisons using population-weighted averages
- U.S. state-level economic analysis
- Statistical analysis (ANOVA, regression, Chi-Square test of independence)
- Downloadable data tables

**Indicators:** GDP per capita, inflation, unemployment, government debt, life expectancy, population growth

### Module 2: Personal Finance Tools

**Features:**
- **Credit Score Analytics (Insights)**: Interactive exploration of credit score relationships using scatter plots, correlation heatmaps, and Random Forest feature importance analysis
- **Savings Projector**: Compound interest calculator with growth projections and personalized savings rate recommendations
- **Loan Calculator**: Amortization schedules, payment analysis, and loan approval probability predictions using gradient descent logistic regression
- **Credit Card Predictor**: Random Forest machine learning model to predict credit card approval probability based on financial profile
- **50/30/20 Financial Guide**: Budget planning tool comparing current spending vs. recommended allocations (needs/wants/savings)
- **Personal Finance Data Explorer**: Browse and download loan approval and credit card datasets used in the application

### Module 3: Retirement Risk Simulator

**Features:**
- Dual-scenario Monte Carlo simulation with 200 simulations per scenario
- Monthly granularity modeling with investment returns and inflation
- Scenario comparison with deterministic projections
- Success rate calculations and portfolio trajectory visualizations

---

## Research Questions

### Module 1: Global Macroeconomic Explorer

1. How do GDP growth and inflation trends vary across global regions over time?
2. What is the relationship between unemployment and inflation across countries? (Phillips curve analysis)
3. How does government debt (% of GDP) relate to economic growth across countries?
4. How do GDP per capita and life expectancy correlate?
5. How do exchange rates behave in countries with high inflation?

### Module 2: Personal Finance Tools

- How do income, savings, debt, and spending patterns relate to credit scores?
- Which financial variables are most important in predicting credit scores?
- What savings rate is needed to reach specific financial goals?
- How do credit profiles affect loan and credit card approval probability?
- What is the total cost of borrowing over different loan terms?

### Module 3: Retirement Risk Simulator

- What is the probability of reaching a retirement goal given current savings and market volatility?
- How do different return scenarios impact long-term portfolio values?
- What is the range of possible retirement outcomes?

---

## Methodology

**Data Collection:** World Bank World Development Indicators (WDI) accessed via WDI R package API, U.S. Census Bureau American Community Survey (ACS), Bureau of Labor Statistics (LAUS and JOLTS), Missouri Economic Research and Information Center (MERIC), Federal Reserve Economic Data (FRED), DataHub.io, and publicly available personal finance training datasets.

**Data Processing:** ETL processes documented in R Markdown files. Data cleaning includes removal of aggregate regions, missing value handling, standardization of country codes, and validation checks.

**Statistical Methods:**

**Descriptive Statistics (mean, median, standard deviation):**
Chosen to provide foundational summaries of economic indicators across countries and time periods. Essential for understanding central tendencies and variability in the World Bank WDI data, which spans 200+ countries over 64 years. Interpretation: Reveals typical economic performance levels, identifies outliers, and quantifies dispersion in indicators like GDP per capita, inflation rates, and unemployment.

**Pearson Correlation Coefficients:**
Selected to measure linear relationships between economic indicators (e.g., GDP and life expectancy, inflation and unemployment). Highly relevant for the World Bank dataset where multiple indicators are tracked simultaneously. Interpretation: Identifies which economic factors move together (positive correlation) or inversely (negative correlation), revealing potential economic relationships such as the wealth-health connection or trade-offs between inflation and unemployment.

**Simple Linear Regression (OLS):**
Chosen to quantify how much variation in a dependent variable (e.g., life expectancy) can be explained by independent economic factors (GDP, unemployment, government debt). Directly applicable to the cross-country panel data structure of the WDI dataset. Interpretation: Provides coefficients indicating the magnitude and direction of relationships, allowing statements like "a $1,000 increase in GDP per capita is associated with X years of life expectancy increase."

**ANOVA for Regional Comparisons:**
Selected to test for statistically significant differences in economic indicators across global regions (e.g., Sub-Saharan Africa vs. North America). Appropriate for the categorical regional groupings inherent in the World Bank data classification. Interpretation: Determines whether observed regional disparities are statistically meaningful or due to random variation, providing evidence for geographic economic inequality.

**Chi-Square Test of Independence:**
Selected to test whether economic indicator categories (e.g., high/low GDP per capita, high/low inflation) are independent of geographic regions. Appropriate for categorical data analysis where countries are classified into economic categories and regional groups. Interpretation: Determines if there is a statistically significant association between economic performance levels and geographic regions. A significant result indicates that certain regions are more likely to have specific economic characteristics, revealing geographic patterns in economic development.

**Monte Carlo Simulation (monthly granularity):**
Selected to model retirement portfolio uncertainty by simulating thousands of possible future scenarios. Appropriate for financial planning where market returns and inflation are stochastic. Interpretation: Provides probability distributions of retirement outcomes, success rates (percentage of simulations where money remains), and confidence intervals. Monthly granularity captures realistic volatility patterns better than annual models.

**Gradient Descent Logistic Regression (loan approval):**
Chosen to predict binary outcomes (loan approval/rejection) based on borrower characteristics. Trained on historical loan approval data to learn patterns in creditworthiness. Interpretation: Provides probability scores for loan approval, helping users understand how factors like income, credit score, and employment history influence approval likelihood. The gradient descent approach allows the model to learn from data without requiring explicit statistical assumptions.

**Random Forest (credit card approval and credit score prediction):**
Selected for both credit card approval prediction and credit score feature importance analysis. Random Forest is an ensemble method that combines multiple decision trees to improve prediction accuracy and provide feature importance metrics. Interpretation: For credit card approval, provides probability scores based on applicant characteristics. For credit score analysis, identifies which financial variables (income, savings, debt ratios, etc.) are most influential in determining credit scores through %IncMSE (percentage increase in mean squared error) metrics.

**Visualization:** Interactive time-series plots, scatterplots, correlation heatmaps, choropleth maps, and dual-axis comparisons using plotly and ggplot2.

---

## Data Sources and Citations

**World Bank World Development Indicators:**
World Bank. (2024). World Development Indicators. The World Bank Group. Available at: https://datatopics.worldbank.org/world-development-indicators/. Data accessed via WDI R package API. License: Creative Commons Attribution 4.0 International (CC BY 4.0)

**U.S. Census Bureau American Community Survey:**
U.S. Census Bureau. (2023). American Community Survey 5-Year Estimates (Tables B19013 - Median Household Income, B17001 - Poverty Status). Retrieved from https://www.census.gov/programs-surveys/acs. License: Public Domain (U.S. Government Work)

**Bureau of Labor Statistics:**
U.S. Bureau of Labor Statistics. Local Area Unemployment Statistics (LAUS). Retrieved from https://www.bls.gov/lau/. License: Public Domain (U.S. Government Work)

U.S. Bureau of Labor Statistics. Job Openings and Labor Turnover Survey (JOLTS) - Job Openings, Hires, and Quits by State. Retrieved from https://www.bls.gov/jlt/. License: Public Domain (U.S. Government Work)

**MERIC Cost of Living Data:**
Missouri Economic Research and Information Center. Cost of Living Data Series. Retrieved from https://meric.mo.gov/data/cost-living-data-series. License: Public Domain (State Government Work)

**Commodity Prices:**
DataHub.io. Core Gold Prices Dataset. Retrieved from https://datahub.io/core/gold-prices. License: Public Domain / Open Data

Federal Reserve Bank of St. Louis. FRED Economic Data - WTI Crude Oil Prices (DCOILWTICO). Retrieved from https://fred.stlouisfed.org/series/DCOILWTICO. License: Public Domain (U.S. Government Work)

**Personal Finance Training Datasets:**
Loan Approval Dataset: Historical loan application records used for training gradient descent logistic regression model. Dataset includes borrower characteristics (income, credit score, loan amount, years employed) and approval outcomes. Source: Publicly available loan approval dataset. Note: For academic use, verify specific dataset source and license compliance.

Credit Card Approval Dataset: Historical credit card application records used for training Random Forest classification model (500 decision trees). Dataset includes applicant demographics, financial information, and approval decisions. Source: Publicly available credit card approval dataset. Note: For academic use, verify specific dataset source and license compliance.

Credit Score Analytics Dataset: Customer financial data including income, savings, debt, spending patterns, and credit scores. Used for Random Forest regression analysis (300 decision trees) to identify key credit score predictors through feature importance metrics. Source: Publicly available credit score dataset. Note: For academic use, verify specific dataset source and license compliance.

---

## Installation and Usage

**Prerequisites:** R (version 4.0 or higher), RStudio (recommended), internet connection

**Setup:**
1. Run the ETL process: Open `data/world_bank_wdi/ETL_world_bank_wdi.Rmd` in RStudio and knit to generate cleaned data
2. Launch the application: Open `ui.R` or `server.R` in RStudio and click "Run App"

The application will automatically install required packages on first run. All data processing is documented in R Markdown files within the `data/` directory.

---

## Disclaimers

**Educational Purpose:** This application is developed for educational purposes as part of BIOL 185 coursework. It is intended for academic learning and demonstration of data science concepts.

**Not Financial Advice:** This application does not constitute financial, investment, or legal advice. All calculations, projections, and analyses are for informational purposes only. Users should consult qualified financial professionals before making any financial decisions. Historical data and simulations do not guarantee future performance.

**AI-Assisted Development:** This codebase was developed with assistance from artificial intelligence tools. While the code has been reviewed and tested, users should verify calculations and methodologies for their specific use cases.

**Data Accuracy:** While efforts have been made to ensure data accuracy, the application relies on external data sources that may contain errors or be subject to revision. Users should verify critical data points from original sources.

---

**Last Updated:** December 2025  
**Version:** 2.1
