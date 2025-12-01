# ============================================================================
# Financial Data Analysis and Planning Dashboard - UI
# BIOL 185 Group Project
# ============================================================================

# Auto-Install and Load Required Packages (UI Check) ----
required_packages <- c("shiny", "bslib", "bsicons", "shinyWidgets", "plotly", "DT", "reshape2")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Load Libraries ----
library(shiny)
library(bslib)          # Modern Bootstrap 5 theming
library(bsicons)        # Bootstrap icons
library(shinyWidgets)
library(plotly)
library(DT)

# Source UI Components ----
source("modules/macro/ui/ui_time_series.R")
source("modules/macro/ui/ui_relationships.R")
source("modules/macro/ui/ui_correlations.R")
source("modules/macro/ui/ui_global_map.R")
source("modules/macro/ui/ui_regional_trends.R")
source("modules/macro/ui/ui_states.R")
source("modules/macro/ui/ui_unemployment.R")
source("modules/macro/ui/ui_commodity.R")
source("modules/macro/ui/ui_statistical_analysis.R")
source("modules/macro/ui/ui_data_table.R")

source("modules/personal_finance/ui/ui_insights.R")
source("modules/personal_finance/ui/ui_savings.R")
source("modules/personal_finance/ui/ui_loans.R")
source("modules/personal_finance/ui/ui_planning_guide.R")
source("modules/personal_finance/ui/ui_credit.R")
source("modules/personal_finance/ui/ui_personal_data.R")

source("modules/retirement/ui/ui_simulator.R")
source("modules/retirement/ui/ui_scenarios.R")

source("ui_components/ui_home.R")
source("ui_components/ui_about.R")

# Define UI with bslib (Modern Web App Structure) ----
ui <- page_navbar(
  # title = tags$span(bs_icon("graph-up-arrow"), " Financial Insight"),
  id = "nav",
  
  # ENTERPRISE LIGHT THEME
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",             
    fg = "#1e293b",             
    primary = "#2563eb",        
    secondary = "#64748b",
    success = "#10b981",
    info = "#0ea5e9",
    warning = "#f59e0b",
    danger = "#ef4444",
    base_font = font_google("Inter"),
    heading_font = font_google("Plus Jakarta Sans"),
    "navbar-bg" = "#1e293b",    
    "navbar-fg" = "#ffffff"     
  ),
  
  # Custom CSS & JavaScript for Dynamic Container Expansion
  header = tags$head(
    tags$title("Financial Insight"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&family=Plus+Jakarta+Sans:wght@600;700&display=swap');
      
      body { background-color: #f1f5f9 !important; }
      .navbar { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .navbar-brand { font-weight: 700; }
      .card { 
        background-color: #ffffff;
        border: 1px solid #cbd5e1; 
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1);
        border-radius: 8px; 
        margin-bottom: 24px;
        overflow: visible !important; /* Allow dropdowns to overflow */
        position: relative;
      }
      .card-header { 
        background-color: #f8fafc; 
        border-bottom: 1px solid #e2e8f0; 
        color: #0f172a;
        font-weight: 600;
        padding: 1rem 1.25rem;
      }
      .card-body {
        overflow: visible !important; /* Allow dropdowns to overflow */
      }
      .form-control, .selectize-input {
        background-color: #ffffff;
        border-color: #cbd5e1;
      }
      .form-control:focus {
        border-color: #2563eb;
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1);
      }
      .btn-primary { box-shadow: 0 1px 2px rgba(0,0,0,0.1); }
      
      /* Selectize Dropdown Styling */
      .selectize-dropdown {
        z-index: 1050 !important; /* Above cards */
        max-height: 400px;
        overflow-y: auto;
      }
      
      /* Ensure parent containers don't clip */
      .col-md-4, .col-md-8, .col-md-3, .col-md-9, .col-4, .col-8, .col-3, .col-9 {
        overflow: visible !important;
      }
    ")),
    
    # JavaScript to dynamically expand containers when dropdowns open
    tags$script(HTML("
      $(document).ready(function() {
        // Function to expand container when dropdown opens
        function expandContainerForDropdown() {
          setTimeout(function() {
            var $dropdown = $('.selectize-dropdown:visible');
            if ($dropdown.length) {
              // Find the selectize input that triggered this dropdown
              var $selectizeInput = $('.selectize-input.focus');
              if ($selectizeInput.length) {
                var $card = $selectizeInput.closest('.card');
                if ($card.length) {
                  var cardBottom = $card.offset().top + $card.outerHeight();
                  var dropdownBottom = $dropdown.offset().top + $dropdown.outerHeight();
                  
                  // If dropdown extends beyond card, expand card
                  if (dropdownBottom > cardBottom) {
                    var extraHeight = dropdownBottom - cardBottom + 30; // 30px padding
                    var currentMinHeight = parseInt($card.css('min-height')) || $card.height();
                    $card.css('min-height', currentMinHeight + extraHeight + 'px');
                  }
                }
              }
            }
          }, 50); // Small delay to ensure dropdown is rendered
        }
        
        // Monitor selectize dropdowns opening
        $(document).on('selectize:open', function() {
          expandContainerForDropdown();
        });
        
        // Also check on click (fallback)
        $(document).on('click', '.selectize-input', function() {
          expandContainerForDropdown();
        });
        
        // Reset card height when dropdown closes
        $(document).on('selectize:close click', function(e) {
          // Only reset if clicking outside
          if (!$(e.target).closest('.selectize').length) {
            $('.card').each(function() {
              var $card = $(this);
              if ($card.data('expanded-height')) {
                $card.css('min-height', '');
                $card.removeData('expanded-height');
              }
            });
          }
        });
        
        // Reset on document click outside
        $(document).on('click', function(e) {
          if (!$(e.target).closest('.selectize').length && !$('.selectize-dropdown:visible').length) {
            $('.card').css('min-height', '');
          }
        });
      });
    "))
  ),

  # 1. Home ----
  nav_panel("Overview", icon = bs_icon("house"), home_ui()),

  # 2. Global Macro (Dropdown) ----
  nav_menu("Global Economy", icon = bs_icon("globe"),
    nav_panel("Time Series Analysis", value = "macro_time_series", icon = bs_icon("graph-up"), time_series_ui()),
    nav_panel("Indicator Relationships", value = "macro_relationships", icon = bs_icon("diagram-2"), relationships_ui()),
    nav_panel("Indicator vs Commodities", value = "macro_commodity", icon = bs_icon("graph-up-arrow"), commodity_ui()),
    nav_panel("Correlation Matrix", value = "macro_correlations", icon = bs_icon("grid-3x3"), correlations_ui()),
    nav_panel("Global Economic Map", value = "macro_map", icon = bs_icon("map"), global_map_ui()),
    nav_panel("Regional Trends", value = "macro_regional", icon = bs_icon("bar-chart-line"), regional_trends_ui()),
    nav_panel("U.S. States", value = "macro_states", icon = bs_icon("geo-alt"), states_ui()),
    nav_panel("State Employment", value = "macro_unemployment", icon = bs_icon("briefcase"), unemployment_ui()),
    nav_panel("Statistical Analysis", value = "macro_statistical", icon = bs_icon("calculator"), statistical_analysis_ui()),
    nav_item(tags$hr()),
    nav_panel("Data Explorer", value = "macro_data", icon = bs_icon("table"), data_table_ui())
  ),

  # 3. Personal Finance (Dropdown) ----
  nav_menu("Personal Finance", icon = bs_icon("wallet2"),
    nav_panel("Insights", value = "pf_insights", icon = bs_icon("search"), insights_ui()),
    nav_panel("Savings Projector", value = "pf_savings", icon = bs_icon("piggy-bank"), savings_ui()),
    nav_panel("Loan Calculator", value = "pf_loans", icon = bs_icon("calculator"), loans_ui()),
    nav_panel("Credit Card Predictor", value = "pf_credit", icon = bs_icon("credit-card"), credit_ui()),
    nav_panel("Financial Guide", value = "pf_guide", icon = bs_icon("journal-text"), planning_guide_ui()),
    nav_panel("Personal Finance Data Explorer", value = "pf_personal_data", icon = bs_icon("table"), personal_data_ui())
  ),

  # 4. Retirement (Dropdown) ----
  nav_menu("Retirement Planning", icon = bs_icon("hourglass-split"),
    nav_panel("Risk Simulator", value = "ret_simulator", icon = bs_icon("activity"), simulator_ui()),
    nav_panel("Scenario Comparison", value = "ret_scenarios", icon = bs_icon("columns-gap"), scenarios_ui())
  ),

  # 5. About ----
  nav_panel("About", icon = bs_icon("info-circle"), about_ui())
  
  # Removed redundant "Data: World Bank" link here
)
