# ============================================================================
# State Employment Data - UI Component
# ============================================================================

unemployment_ui <- function() {
  fluidRow(
    column(12,
           card(
             card_header("State Employment Data - Interactive Map"),
             div(class = "p-3",
                 p(tags$b("This dataset provides comprehensive state-level employment statistics from the Bureau of Labor Statistics' Job Openings and Labor Turnover Survey (JOLTS). The data tracks job market dynamics through job openings, hires, and voluntary separations (quits) across all 50 U.S. states.")),
                 tags$h5("Data Fields Explanation:"),
                 tags$ul(
                   tags$li(tags$b("Job Opening Rate:"), " Percentage of total employment plus job openings that are unfilled positions. Higher rates indicate more available opportunities relative to workforce size."),
                   tags$li(tags$b("Job Opening Levels:"), " Total number of unfilled job positions (in thousands) available at the end of the month. Represents employer demand for workers."),
                   tags$li(tags$b("Hiring Rate:"), " Monthly hires as a percentage of total employment. Measures the pace at which employers are filling positions."),
                   tags$li(tags$b("Hiring Levels:"), " Total number of workers hired (in thousands) during the month. Includes new and replacement hires."),
                   tags$li(tags$b("Quitting Rate:"), " Voluntary separations as a percentage of total employment. Often viewed as an indicator of worker confidence in finding new jobs."),
                   tags$li(tags$b("Quitting Levels:"), " Total number of workers (in thousands) who voluntarily left their jobs during the month, excluding retirements.")
                 )
             )
           )
    ),
    
    column(3,
           card(
             card_header("Map Settings"),
             selectizeInput("map_metric_employment", "Select Metric:",
                            choices = c(
                              "Job Opening Rate" = "job_open_rate",
                              "Job Openings (Levels)" = "job_open_level",
                              "Hiring Rate" = "hiring_rate",
                              "Hires (Levels)" = "hiring_level",
                              "Quitting Rate" = "quitting_rate",
                              "Quits (Levels)" = "quitting_level"
                            ),
                            selected = "job_open_rate",
                            options = list(dropdownParent = 'body')),
             
             selectizeInput("time_period_employment", "Select Time Period:",
                            choices = c(
                              "July 2025" = "july_2025",
                              "June 2025" = "june_2025",
                              "May 2025" = "may_2025",
                              "April 2025" = "april_2025",
                              "July 2024" = "july_2024"
                            ),
                            selected = "july_2025",
                            options = list(dropdownParent = 'body'))
           ),
           
           card(
             card_header("Metric Information"),
             p(style = "font-size: 13px; color: #64748b; margin-top: 10px;",
               textOutput("metric_explanation_employment"))
           ),
           
           card(
             card_header("Top 5 States"),
             p(style = "font-size:12px; color:#64748b; margin-bottom:10px;",
               "States with the highest values for the selected metric:"),
             tableOutput("top_states_employment")
           ),
           
           card(
             card_header("Bottom 5 States"),
             p(style = "font-size:12px; color:#64748b; margin-bottom:10px;",
               "States with the lowest values for the selected metric:"),
             tableOutput("bottom_states_employment")
           )
    ),
    
    column(9,
           card(
             card_header("Interactive Employment Map"),
             p(style = "font-size:13px; color:#64748b; margin-bottom:10px;",
               "Hover over states to see detailed statistics. Use the dropdown menus on the left to change the metric and time period displayed."),
             plotlyOutput("employment_map_other", height = "550px")
           ),
           
           card(
             card_header("Data Source Information"),
             accordion(
               accordion_panel(
                 title = "Bureau of Labor Statistics (BLS) - JOLTS",
                 tags$ul(
                   tags$li(tags$b("Source:"), " Job Openings and Labor Turnover Survey (JOLTS)"),
                   tags$li(tags$b("Coverage:"), " All 50 U.S. states, monthly data"),
                   tags$li(tags$b("Collection Method:"), " Survey of approximately 21,000 establishments covering all industries"),
                   tags$li(tags$b("URL:"), tags$a(href = "https://www.bls.gov/jlt/", target = "_blank", "https://www.bls.gov/jlt/")),
                   tags$li(tags$b("Data Period:"), " July 2024 - July 2025"),
                   tags$li(tags$b("Release Schedule:"), " Monthly, approximately 6 weeks after reference month")
                 )
               ),
               accordion_panel(
                 title = "Definitions & Methodology",
                 tags$ul(
                   tags$li(tags$b("Job Openings:"), " Positions that are open (not filled) on the last business day of the month, could start within 30 days, and employer is actively recruiting"),
                   tags$li(tags$b("Hires:"), " All additions to payroll during the month, including new employees and recalls"),
                   tags$li(tags$b("Quits:"), " Voluntary separations initiated by the employee (excludes layoffs, discharges, retirements, transfers, and deaths)"),
                   tags$li(tags$b("Rates:"), " Calculated as the number divided by employment plus job openings (for openings rate) or employment (for hires/quits rates), then multiplied by 100")
                 )
               ),
               accordion_panel(
                 title = "Data Limitations",
                 tags$ul(
                   tags$li("State-level JOLTS data has higher sampling variability than national estimates"),
                   tags$li("Some months may show volatility due to seasonal factors or one-time events"),
                   tags$li("Data represents establishment-based counts, not individual workers"),
                   tags$li("Preliminary estimates may be revised in subsequent releases")
                 )
               )
             )
           )
    )
  )
}