#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  ### Title "Muskingum Application"
  
  shiny::fluidRow(
    shiny::titlePanel("Unit Hydrograph")
  ),
  
  
  ### Data input
  
  h2("Data input"),

  shiny::fluidRow(
    col_4(
      shinydashboard::box(
        title = "Effective precipitation", width = 12,
        p("Table 1: Effective precipitation [mm/h] as system-input."),
        rhandsontable::rHandsontableOutput("ui_SystemInput")
      ),
      shinydashboard::box(
        title = "", width = 12, height = 203,
        shiny::uiOutput("ui_area"),
        shiny::uiOutput("ui_bfslider")
      ),
      col_4(
        shiny::actionButton("clear_input", "Clear input")
      ), col_5(
        shiny::actionButton("demo_data", "Use demo data")
      ), col_3(
        shiny::actionButton("run", "Run", class = "btn-warning")
      )
    ),
    col_3(
      shinydashboard::box(
        title = "Discharge", width = 12,
        p("Table 2: System-output in [m^3/s]."),
        rhandsontable::rHandsontableOutput("ui_SystemOutput")
      )
    ),
    col_5(
      shiny::plotOutput("ui_datainput_plot")
    )
  ),
  
  ### Calculations
  
  shiny::uiOutput("ui_calculations"),
  
  ### Unit Hydrograph
  
  shiny::uiOutput("ui_unithydrograph"),

)
