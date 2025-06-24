#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  ### Values
  
  input_vals = shiny::reactiveValues(
    SystemInput = demo_SystemInput,
    SystemOutput = demo_SystemOutput,
    area = demo_area,
    bf = demo_bf,
    rain = demo_rain
  )
  
  vals = shiny::reactiveValues(
    SystemInput = demo_SystemInput,
    SystemOutput = demo_SystemOutput,
    area = demo_area,
    bf = demo_bf,
    rain = demo_rain
  )
  
  ### Serverlogic
  
  ### 1. Input stuff
  
  shiny::observe({
    if (!is.null(input$ui_SystemInput)) {
      input_vals[["SystemInput"]] = rhandsontable::hot_to_r(input$ui_SystemInput)
    }
  })
  
  shiny::observe({
    if (!is.null(input$ui_SystemOutput)) {
      input_vals[["SystemOutput"]] = rhandsontable::hot_to_r(input$ui_SystemOutput)
    }
  })
  
  shiny::observe({
    if (!is.null(input$ui_rain)) {
      input_vals[["rain"]] = rhandsontable::hot_to_r(input$ui_rain)
    }
  })
  
  shiny::observeEvent(input$demo_data, {
    input_vals$SystemInput = demo_SystemInput
    input_vals$SystemOutput = demo_SystemOutput
    input_vals$area = demo_area
    input_vals$bf = demo_bf
    input_vals$rain = demo_rain
  })

  shiny::observeEvent(input$clear_input, {
    input_vals$SystemInput = empty_df(r = 5, c = 2, cn = c("t", "P_eff"))
    input_vals$SystemOutput = empty_df(r = 17, c = 2, cn = c("t", "Q"))
    input_vals$area = 0
    input_vals$bf = NA
    input_vals$rain = empty_df(r = 3, c = 2, cn = c("t", "P_eff"))
    vals$SystemInput = NULL
    vals$SystemOutput = NULL
    vals$area = NULL
    vals$bf = NULL
  })
  
  shiny::observeEvent(input$run, {
    vals$SystemInput = input_vals$SystemInput
    vals$SystemOutput = input_vals$SystemOutput
    vals$area = input_vals$area
    vals$bf = input_vals$bf
  })
  
  dt = shiny::reactive(
    unique(diff(input_vals$SystemOutput$t))
  )
  
  ### 2. Methode
  
  ### 2.1 Basisabfluss und Direktabfluss
  
  t = shiny::reactive(
    if (!is.null(vals$SystemOutput)) {
      vals$SystemOutput$t
    }
  )
  
  Q_B = shiny::reactive({
    if (!is.null(vals$SystemOutput)) {
      q = vals$SystemOutput
      q$Q_B = q$Q
      q$Q_B[q$t > input$bfslider[1] & q$t < input$bfslider[2]] = NA
      q$Q_B = data.frame(zoo::na.approx(q, x = q$t))$Q_B
      return(q$Q_B)
    }
  })
  
  Q_D = shiny::reactive({
    if (!is.null(Q_B())) {
      qd = vals$SystemOutput$Q - Q_B()
      f = 3600 * 1000/(vals$area * 10**6) 
      return(qd*f)
    }
  })
  
  ### 2.2 Überprüfen der Volumina
  
  # Volumen P_eff
  V_Peff = shiny::reactive(
    sum(vals$SystemInput$P_eff) * vals$area * 1000 * dt()
  )
  
  # Volumen Q_d
  V_QD = shiny::reactive(
    sum(sapply(X = 1:(length(Q_D())-1), FUN = function(i) {
      0.5 * (Q_D()[i] + Q_D()[i+1])
    })) * vals$area * 1000 * dt()
  )
  
  V_quot = shiny::reactive(
    V_Peff()/V_QD()
  )
  
  ### 2.3 Systemidentifikation

  # Niederschlagsordinaten  
  precip = shiny::reactive({
    precip = vals$SystemInput$P_eff
    precip[precip == 0.0] = NA
    return(vals$SystemInput$P_eff[!is.na(zoo::na.approx(precip, na.rm = FALSE))])
  })
  
  # Anzahl der Niederschlagsordinaten
  m = shiny::reactive(length(precip()))
  
  # Abflussordinaten
  disch = shiny::reactive({
    disch = Q_D()
    disch[disch == 0.0] = NA
    return(Q_D()[!is.na(zoo::na.approx(disch, na.rm = FALSE))])
  })
  
  # Anzahl der Ordinaten des Abflusses
  r = shiny::reactive({length(disch())})
  
  # Anzahl UH - Ordinaten
  n = shiny::reactive({r() - m() + 1})
  
  # UH-Matrix
  UHM = shiny::reactive({
    # Leere Matrix
    uhm = matrix(0, nrow = r(), ncol = m())
    # Auffüllen
    for (i in 1:m()) {
      uhm[i:(i+n()-1),i] = rep(vals$SystemInput$P_eff[i], n())
    }
    return(uhm)
  }) 
  
  # Näherungslösung mit Hilfe kleinster Quadrate
  
  sq = shiny::reactive({
    lsq(disch(), UHM(), precip(), k_ = n(), m = m())
  })
  
  U = shiny::reactive({
    df = data.frame(
      row.names = paste0("U_", 1:n()),
      U = solve(sq()$A, sq()$b)
    )
    df$U_norm = df$U * 1/sum(df$U) * 1/dt()
    return(df)
  })
  
  ### 2.4 Faltung
  
  # Berechnung Niederschagsdauer
  rain = shiny::reactive({
    precip = input_vals$rain$P
    precip[precip == 0.0] = NA
    return(input_vals$rain$P[!is.na(zoo::na.approx(precip, na.rm = FALSE))])
  })
  
  n_rain = shiny::reactive(length(rain()))
  
  
  folds = shiny::reactive({
    folds = matrix(unlist(lapply(input_vals$rain$P, function(p) {
      p * U()$U_norm * dt()
    })), nrow = nrow(input_vals$rain), byrow = TRUE)
    
    B = matrix(0, nrow = nrow(folds) + ncol(folds), ncol = ncol(folds))
    
    for (i in 1:ncol(folds)) {
      B[i:(i+n_rain()-1), i] = folds[1:n_rain(),i]
    }
    return(B)
  })
  
  discharge = shiny::reactive({
    t = 1:nrow(folds()) * dt()
    r = input_vals$rain
    f = 3600 * 1000/(vals$area * 10**6)
    Q = rowSums(folds()) * 1/f
    
    d = data.frame(
      t = t,
      r = rep(0, length(t)),
      Q = Q
    )
    d$r[1:length(r)] = r
    
    print(d)
    return(d)
  })

  ### Output
  output$ui_SystemInput <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["SystemInput"]]))
      rhandsontable::rhandsontable(input_vals[["SystemInput"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_SystemOutput <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["SystemOutput"]]))
      rhandsontable::rhandsontable(input_vals[["SystemOutput"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_datainput_plot = shiny::renderPlot(
    if (!is.null(input_vals[["SystemInput"]])) {
      tryCatch({
        return(datainput_plot(inp = input_vals[["SystemInput"]], outp = input_vals[["SystemOutput"]], bf = input$bfslider))
      }, error = function(e) {
        return(plot.new())
      })
    },
    height = 520
  )
  
  output$ui_area = shiny::renderUI(
    shiny::numericInput(
      inputId = "area",
      label ="Area [km^2]",
      value = input_vals$area
    )
  )
  
  output$ui_bfslider = shiny::renderUI({
    if (!any(is.na(input_vals$bf))) {
      shiny::sliderInput( 
        "bfslider", 
        "Seperate Baseflow", 
        min = min(input_vals[["SystemOutput"]]$t), max = max(input_vals[["SystemOutput"]]$t), 
        value = input_vals$bf,
        step = dt(),
      )
    }
  })
  
  output$ui_table = shiny::renderTable(
    if (!is.null(vals$SystemInput)) {
      return(U())
    }
  )
  
  output$ui_Q_table = shiny::renderTable(
    data.frame(
      t = t(),
      Q = vals$SystemOutput$Q,
      Q_B = Q_B(),
      Q_D = Q_D()
    )
  )
  
  output$ui_disch = shiny::renderTable(
    data.frame(d = disch()), 
    colnames = FALSE, 
    bordered = TRUE,
    digits = 3,
    width = 12
  )
  
  output$ui_UHM = shiny::renderTable(
    UHM(), 
    colnames = FALSE, 
    bordered = TRUE,
    digits = 3,
    width = 12
  )
  
  output$ui_lsqA = shiny::renderTable(
    sq()$A,
    colnames = FALSE, 
    bordered = TRUE,
    digits = 2
  )
  
  output$ui_lsqb = shiny::renderTable(
    sq()$b,
    colnames = FALSE, 
    bordered = TRUE,
    digits = 3
  )
    
  output$ui_calculations = shiny::renderUI(
    if (!is.null(vals$SystemInput)) {
      shiny::tagList(
        h2("Calculations"),
        shiny::fluidRow(
          col_4(
            shinydashboard::box(
              title = "Discharge separation", width = 12,
              shiny::tableOutput("ui_Q_table")
            )
          ),
          col_8(
            shinydashboard::box(
              title = "Volume contoll", width = 12,
              shiny::HTML(paste("Volume of effective preciptiation: <b>", V_Peff(), "m^3</b><br>")),
              shiny::HTML(paste("Volume of direct discharge: <b>", V_QD(), "m^3</b><br>")),
              shiny::HTML(paste("Ratio: <b>", V_Peff()/V_QD(), "</b><br>"))
            ),
            
            shinydashboard::box(
              title = "UH-Matrix", width = 12,
              div(shiny::tableOutput("ui_disch"), style="float:left"),
              div(shiny::tableOutput("ui_UHM"), style="margin-left:0.5%; float:left")
            )
          ),
          col_12(
            shinydashboard::box(
              title = "LSQ-Matrix", width = 12,
              div(shiny::tableOutput("ui_lsqA"), style="float:left"),
              div(shiny::tableOutput("ui_lsqb"), style="margin-left:0.5%; float:left")
            )
          )
        )
      )
    }
  )
  
  output$ui_U = shiny::renderTable(
    U(),
    digits = 4,
    rownames = TRUE
  )
  
  output$ui_rain <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["rain"]]))
      rhandsontable::rhandsontable(input_vals[["rain"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_dischargeresponse = shiny::renderPlot({
    datainput_plot(inp = input_vals$rain, outp = discharge())
  })
  
  output$ui_unithydrograph = shiny::renderUI(
    if (!is.null(vals$SystemInput)) {
      shiny::tagList(
        h2("Results"),
        shiny::fluidRow(
          col_3(
            shinydashboard::box(
              title = "UH-Ordinates", width = 12,
              div(shiny::tableOutput("ui_U"), style="float:left")
            )
          ),
          col_3(
            shinydashboard::box(
              title = "Rainfall event", width = 12,
              p("Table 3: Effective precipitation [mm/h] over time [h]."),
              rhandsontable::rHandsontableOutput("ui_rain")
            )
          ),
          col_6(
            shinydashboard::box(
              title = "Discharge response", width = 12,
              p("Figure 2: Discharge response based on UH."),
              shiny::plotOutput("ui_dischargeresponse", height = 300)
            )
          )
        )
      )
    }
  )
  
}
