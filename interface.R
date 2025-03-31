ui <- fluidPage(
  includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # App title ----
  titlePanel("Preferences"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      
      fileInput("file_FD",  "Case Data", buttonLabel = "Upload..."),
      textInput("delim", "Delimiter (Blank = Best Guess)", ";"),
      textInput("dec", "Decimal Separator", "."),
      
    ),
    
    # Main panel for displaying outputs ----
    
    mainPanel(
      fluidRow(
        
        column(4,
               
               actionButton("show",
                            "Help and Instructions",
                            style = "color: #000000; background-color: #c42442; border-color: #c42442"),
               
               downloadButton("dwnld", "Compile Report"),
               
               div(HTML('<br/>')),
               
               h3("The model is shown in this diagram"),
               div(HTML('<img src="eidm.png" width="100%" height="100%">')),# width = 240 alt="COMBACTE logo" style="margin-top: 25px">')),
               
               div(HTML('<br/>')),
               div(HTML('<br/>')),
               div(HTML('<br/>')),
               div(HTML('<br/>')),
               div(HTML('<br/>')),
               div(HTML('<br/>')),
               
               conditionalPanel("output.show", checkboxInput("check_patho", "Check Data")),
               conditionalPanel("output.show", checkboxInput("check_cox", "Check Data")),
               conditionalPanel("output.show", checkboxInput("check_data_box", "Check Data")),
               conditionalPanel("output.show", checkboxInput("check_raw_data_box", "Check Data"))
               
        ),
        
        column(3,
               h3("Subgroups"),
               
               selectInput("intermediate_event",
                           "Considered Pathogen",
                           ""),
               
               sliderInput("age",
                           "Age:",
                           min   = 0,
                           max   = 99,
                           value = c(0, 99),
                           step  = 10),
               
               selectInput("sex",
                           "Sex:",
                           c("All", "f", "m", "Non-Binary")),
        ),
        conditionalPanel(
          condition = ("!input.check_raw_data_box"),
          column(3,
                 
                 sliderInput("time_max",
                             "max Time:",
                             min   = 0,
                             max   = 100,
                             value = 10,
                             step  = 1),
                 
                 # style = 'display: flex; justify-content: center',
                 uiOutput("table_1")
          )
        )
        
      ),

    )
  ),
  
  fluidRow(column(4,
                  tableOutput("test")
                  )
  ),
  
  fluidRow(column(4,
                  h3("Plot Order and Coloring"),
                  h4("from top to bottom")
                  )
  ),
  
  fluidRow(column(2,
                  selectInput("state_1",
                              "First Area",
                              ""),
                  colourpicker::colourInput("col_state_1",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[3],
                              showColour = "background")),
           column(2,
                  selectInput("state_2",
                              "Second Area",
                              ""),
                  colourpicker::colourInput("col_state_2",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[6],
                              showColour = "background")),
           column(2,
                  selectInput("state_3",
                              "Third Area",
                              ""),
                  colourpicker::colourInput("col_state_3",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[4],
                              showColour = "background")),
           column(2,
                  selectInput("state_4",
                              "Fourth Area",
                              ""),
                  colourpicker::colourInput("col_state_4",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[1],
                              showColour = "background")),
           column(2,
                  selectInput("state_5",
                              "Fifth Area",
                              ""),
                  colourpicker::colourInput("col_state_5",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[5],
                              showColour = "background")),
           column(2,
                  selectInput("state_6",
                              "Sixth Area",
                              ""),
                  colourpicker::colourInput("col_state_6",
                              "",
                              as.vector(palette.colors(palette = "Okabe-Ito"))[2],
                              showColour = "background"))),
  
  fluidRow(column(4,
                  h3("Plots")
                  )
  ),
  
  
  
  conditionalPanel(
    condition = ("!input.check_raw_data_box && !input.check_data_box"),
    fluidRow(column(6,
                    h4("Starting w/o Intermediate"),
                    plotOutput("Plot1"),
                    tableOutput("tabletrans_1")),
             column(6,
                    h4("Starting w/ Intermediate"),
                    plotOutput("Plot2"),
                    tableOutput("tabletrans_2"),
                    tableOutput("clos")
                    )
    ),
    
    
    
    fluidRow(
      
      conditionalPanel(
        condition = ("!input.check_patho"),
        column(6,
               h4("Pathogens"),
               plotOutput("circular",
                          height = "1000px")
        )
      ),
      
      conditionalPanel(
        condition = ("input.check_patho"),
        column(6,
               h4("Pathogens"),
               h5("no Pathogens defined")
        )
      ),
      
      conditionalPanel(
        condition = ("!input.check_cox"),
        column(4,
               h4("Discharge"),
               h5("Effect of an intermediate event on discharge, corrected for sex and age"),
               plotOutput("Plot_cox_1"),
               h4("Death"),
               h5("Effect of an intermediate event on death, corrected for sex and age"),
               plotOutput("Plot_cox_2")
        )
      ),
      
      
      conditionalPanel(
        condition = ("input.check_cox"),
        column(6,
               h4("Discharge"),
               h5("To less Data"),
               h4("Death"),
               h5("To less Data")
        )
      ),
      
      
    )
  ),
  
  conditionalPanel(
    condition = ("input.check_data_box"),
    fluidRow(column(2,
                    h3("To few data")))
  ),
  
  conditionalPanel(
    condition = ("input.check_raw_data_box"),
    fluidRow(column(6,
                    h3("Data Error"),
                    div(HTML('Possible Reasons are:<br>
                             <ul>
                                 <li>Wrong Delimeter</li>
                                 <li>Wrong Decimal separator</li>
                                 <li>Missing Variables</li>
                                 <li>Wrong Date format</li>
                                 <li>Dates do not match (i.e. Discharge before Admission)</li>
                                 <li>Missing necessary dates (Admission Date, Discharge Date, Discharge Reason)</li>
                                 <li>To few transitions</li>
                             </ul>'))))
  )
    
)


server <- function(input, output, session) {
  values <- reactiveValues()
  values$show <- FALSE
  
  # Help ---------------------------------------------------------
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Notes and Help",
      size = "l",
      div(HTML('This is a tool to visualize time-dependend effects in a multi-state-setting considering an extended illness-death model <br>
      The model hase the following structure:<br>')),
      
      tags$img(
        src   = "eidm.png",
        width = 600,
        alt   = "The extended illness death model with constant transition hazards"),
      
      div(HTML('<ul>
        <li>One data file can be uploaded. An example file can be found here:<br>
        <a href="data_example.csv"> <button class="btn"><i class="fa fa-download"></i> Download</button> </a></li><br>
        <li>This data file have to be in the csv format and have to contain the following variables<br>
            <ul>
               <li>"admission_date": Date of Admission to the Hospital/ICU/ECMO...</li>
               <li>"intermediate_date": Date of the occurance of the interemdiate event. Enter "NA" or leave blank if there have not been an intermediate event for the corresponding patient</li>
               <li>"discharge_date": Date the patient leaves the Hospital/ICU/ECMO bei discharge either alive or dead</li>
               <li>"discharge_status": "a" for alive, "d" for dead</li>
               <li>"pathogen": If there have been involved some kind of pathogens for the intermediate event this variable can define the specific pathogen. Enter "NA" or leave blank if there have not been a pathogen</li>
               <li>"age": The Age in years of the Patient. Enter "NA" or leave blank if unknown</li>
               <li>"sex": "f" for female, "m" for male. Enter "NA" or leave blank if unknown
            </ul></li>
        <li>Date formats have to in the the format "YYYY-MM-DD"</li>
        <li>A delimeter and a decimal separator can be set. Standard Delimeter is ";" and standard separator is "."</li>
        <li>Some Settings can be made:<br>
            <ul>
               <li>Max Time: the time horizon can be set</li>
               <li>Considered Pathogen: If one wants to have these plots for a specific pathogen this can be chosen here</li>
               <li>Age: The Dataset can be restricted to a specific range in age</li>
               <li>Sex: If a specific sex shoul dbe consideres this can be set here</li>
            </ul>
        <li>Some additional setting concerning the layout of the plots are included<br>
            <ul>
               <li>State 1 to 6: The order of the Stacks can be set by choosing the specific state at the specific position between 1 and 6</li>
               <li>The colored bars underneathe the State menu can be used to alter the color of the corresponding state</li>
            </ul></li>
        <li>The stacked probability plots it self represents the probability to be in a specific state at every point in time and therefore shows the statistical path through the  states<br>
        <img src="spp.png" alt="Example for a stacked probability plot"></li>
        <li>Beneath the plots are tables showing the state occupation probability for the last shown point in time of these plots</li>
        <li>Also shown is the change in length of stay</li>
        <li>A circular plot shows the distribution of the pathogens</li>
        <li>Two cox regressions show the influence of the intermediate event, age and sex and the phases of the pandemic on the two outcomes discharge and death</li>
        <li>There is also an option to generate a report including all the grafics and tables shown in the app.<br>
            In order to use this functionality one needs to:
          <ul>
            <li>install a Latex Distribution, e.g. <a href="https://miktex.org/">Miktex</a></li>
            <li>install R Markdown</li>
          </ul>
        </ul>
        ')),
      easyClose = TRUE
    ))
  })
  
  # Upload ---------------------------------------------------------
  raw_FD <- reactive({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    req(input$file_FD)
    delim <- if (input$delim == "") ";" else input$delim
    dec   <- if (input$dec   == "") "." else input$dec
    
    data_FD <- vroom::vroom(file            = input$file_FD$datapath,
                            guess_max = Inf,
                            delim           = delim,
                            skip_empty_rows = TRUE,
                            locale          = locale(decimal_mark = dec))
    
    if(check_raw_func(data_FD)) {
      data_FD <- data.frame(id                = c("error", 1:3),
                            admission_date    = c("2020-01-01",
                                                  "2020-01-01",
                                                  "2020-01-01",
                                                  "2020-01-01"),
                            intermediate_date = c("2020-01-02",
                                                  "2020-01-03",
                                                  NA,
                                                  NA),
                            discharge_date    = c("2020-01-04",
                                                  "2020-01-05",
                                                  "2020-01-06",
                                                  "2020-01-07"),
                            discharge_status  = c("d", "a", "d", "a"),
                            pathogen          = rep(NA, 4),
                            age               = rep(18, 4),
                            sex               = rep("f", 4))
    }
    
    if(is.logical(data_FD$sex)) {
      
      data_FD$sex <- as.factor(ifelse(!data_FD$sex,
                                      "f",
                                      "m"))
      
    }
    
    if(!is.numeric(data_FD$age)) {
      
      data_FD$age <- rep(NA, nrow(data_FD))
      
    }
    
    data_FD$sex <- as.factor(as.character(data_FD$sex))
    
    data_FD
    
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    updateSelectInput(session,
                      "intermediate_event",
                      choices = c("All", unique(raw_FD()$pathogen)[!is.na(unique(raw_FD()$pathogen))]),
                      selected = "All")
    
    updateSliderInput(session,
                      "time_max",
                      max   = as.numeric(max(as.Date(raw_FD()$discharge_date) - as.Date(raw_FD()$admission_date))),
                      value = min(100, max(as.Date(raw_FD()$discharge_date) - as.Date(raw_FD()$admission_date))))
    
    updateSliderInput(session,
                      "age",
                      min   = ifelse(!is.numeric(raw_FD()$age), 1, min(raw_FD()$age)),
                      max   = ifelse(!is.numeric(raw_FD()$age), 999, max(raw_FD()$age)))
    
    updateSelectInput(session,
                      "sex",
                      choices = if(all(is.na(raw_FD()$sex)) | (n_distinct(raw_FD()$sex) == 1)){
                        "All"
                      }else {
                        c("All", as.character(unique(raw_FD()$sex)))
                      },
                      selected = "All")
    
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_1",
                      choices  = states,
                      selected = states[1])
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_2",
                      choices  = states[!(states %in% input$state_1)],
                      selected = states[!(states %in% input$state_1)][1])
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_3",
                      choices  = states[!(states %in% c(input$state_1, input$state_2))],
                      selected = states[!(states %in% c(input$state_1, input$state_2))][1])
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_4",
                      choices  = states[!(states %in% c(input$state_1, input$state_2, input$state_3))],
                      selected = states[!(states %in% c(input$state_1, input$state_2, input$state_3))][1])
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_5",
                      choices  = states[!(states %in% c(input$state_1, input$state_2, input$state_3, input$state_4))],
                      selected = states[!(states %in% c(input$state_1, input$state_2, input$state_3, input$state_4))][1])
  })
  
  observe({
    
    id <- showNotification(
      "Rendering...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    states <- c("Discharge w/o Int.",
                "Deceased w/o Int.",
                "Admitted",
                "Intermediate",
                "Discharge w/ Int.",
                "Deceased w/ Int.")
    
    updateSelectInput(session,
                      "state_6",
                      choices  = states[!(states %in% c(input$state_1, input$state_2, input$state_3, input$state_4, input$state_5))],
                      selected = states[!(states %in% c(input$state_1, input$state_2, input$state_3, input$state_4, input$state_5))][1])
    
  })
  
  
  output$Plot1 <- renderPlot({

    id <- showNotification(
      "Rendering...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)

    plot_trans(trans_data(data_FD        = raw_FD(),
                          inf            = input$intermediate_event,
                          sex            = input$sex,
                          age            = input$age),
               color_scheme = c(input$col_state_1,
                                input$col_state_2,
                                input$col_state_3,
                                input$col_state_4,
                                input$col_state_5,
                                input$col_state_6),
               ord        = c(input$state_1,
                              input$state_2,
                              input$state_3,
                              input$state_4,
                              input$state_5,
                              input$state_6),
               time_max = input$time_max)[[1]]


  })
  
  output$tabletrans_1 <- renderTable(
    
    plot_trans(trans_data(data_FD        = raw_FD(),
                          inf            = input$intermediate_event,
                          sex            = input$sex,
                          age            = input$age),
               color_scheme = input$color_scheme,
               time_max = input$time_max)[[2]]
  )
  
  output$test <- renderTable(
    check_raw_func(data_FD)
    # check_func(trans_data(data_FD        = raw_FD(),
    #                       inf            = input$intermediate_event,
    #                       sex            = input$sex,
    #                       age            = input$age))

  )
  
  observe({
    
    updateCheckboxInput(session,
                        "check_data_box",
                        value = check_func(trans_data(data_FD        = raw_FD(),
                                                      inf            = input$intermediate_event,
                                                      sex            = input$sex,
                                                      age            = input$age)))
  })
  
  observe({
    
    updateCheckboxInput(session,
                        "check_cox",
                        value = check_cox(raw_FD(), input$intermediate_event))
    
  })
  
  observe({
    
    updateCheckboxInput(session,
                        "check_patho",
                        value = all(is.na(raw_FD()$pathogen)))
  })
  
  observe({
    
    updateCheckboxInput(session,
                        "check_raw_data_box",
                        value = is.character(raw_FD()$id))
  })
  
  output$Plot2 <- renderPlot({

    id <- showNotification(
      "Rendering...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)

    plot_trans_2(trans_data(data_FD        = raw_FD(),
                            inf            = input$intermediate_event,
                            sex            = input$sex,
                            age            = input$age),
                 color_scheme = c(input$col_state_1,
                                  input$col_state_2,
                                  input$col_state_3,
                                  input$col_state_4,
                                  input$col_state_5,
                                  input$col_state_6),
                 ord        = c(input$state_1,
                                input$state_2,
                                input$state_3,
                                input$state_4,
                                input$state_5,
                                input$state_6),
                 time_max   = input$time_max)[[1]]


  })
  
  output$tabletrans_2 <- renderTable(

    plot_trans_2(trans_data(data_FD        = raw_FD(),
                            inf            = input$intermediate_event,
                            sex            = input$sex,
                            age            = input$age),
                 color_scheme = input$color_scheme,
                 time_max = input$time_max)[[2]])
  
  output$clos <- renderTable(

    clos_tab(trans_data(data_FD        = raw_FD(),
                        inf            = input$intermediate_event,
                        sex            = input$sex,
                        age            = input$age)))
  
  output$Plot_cox_1 <- renderPlot({

    id <- showNotification(
      "Rendering...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    mdl <- model_cox(data_cox(trans_data(data_FD  = raw_FD(),
                                         inf      = input$intermediate_event)),
                     3)
    
    forest_cox(mdl)

  })
  
  output$Plot_cox_2 <- renderPlot({

    id <- showNotification(
      "Rendering...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    mdl <- model_cox(data_cox(trans_data(data_FD  = raw_FD(),
                                         inf      = input$intermediate_event)),
                     4)
    
      forest_cox(mdl)
      
  })
  
  output$circular <- renderPlot({

    id <- showNotification(
      "Rendering...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)

    circular_plot(data_FD = raw_FD())

  })

  output$table_1 <- renderUI({
    HTML(paste(render_table_1(data_FD = raw_FD())))
  })
  
  output$dwnld <- downloadHandler(
    filename = function() {
      paste0("report", Sys.Date(), ".pdf")
    },
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        params = list(Plot1        = plot_trans(trans_data(data_FD        = raw_FD(),
                                                                           inf            = input$intermediate_event,
                                                                           sex            = input$sex,
                                                                           age            = input$age),
                                                                color_scheme = c(input$col_state_1,
                                                                                 input$col_state_2,
                                                                                 input$col_state_3,
                                                                                 input$col_state_4,
                                                                                 input$col_state_5,
                                                                                 input$col_state_6),
                                                                ord        = c(input$state_1,
                                                                               input$state_2,
                                                                               input$state_3,
                                                                               input$state_4,
                                                                               input$state_5,
                                                                               input$state_6),
                                                                time_max = input$time_max)[[1]],
                                      tabletrans_1 = plot_trans(trans_data(data_FD        = raw_FD(),
                                                                           inf            = input$intermediate_event,
                                                                           sex            = input$sex,
                                                                           age            = input$age),
                                                                color_scheme = input$color_scheme,
                                                                time_max = input$time_max)[[2]],
                                      Plot2        = plot_trans_2(trans_data(data_FD        = raw_FD(),
                                                                             inf            = input$intermediate_event,
                                                                             sex            = input$sex,
                                                                             age            = input$age),
                                                                  color_scheme = c(input$col_state_1,
                                                                                   input$col_state_2,
                                                                                   input$col_state_3,
                                                                                   input$col_state_4,
                                                                                   input$col_state_5,
                                                                                   input$col_state_6),
                                                                  ord        = c(input$state_1,
                                                                                 input$state_2,
                                                                                 input$state_3,
                                                                                 input$state_4,
                                                                                 input$state_5,
                                                                                 input$state_6),
                                                                  time_max   = input$time_max)[[1]],
                                      tabletrans_2 = plot_trans_2(trans_data(data_FD        = raw_FD(),
                                                                             inf            = input$intermediate_event,
                                                                             sex            = input$sex,
                                                                             age            = input$age),
                                                                  color_scheme = input$color_scheme,
                                                                  time_max = input$time_max)[[2]],
                                      clos         = clos_tab(trans_data(data_FD        = raw_FD(),
                                                                         inf            = input$intermediate_event,
                                                                         sex            = input$sex,
                                                                         age            = input$age)),
                                      Plot_cox_1   = forest_cox(model_cox(data_cox(trans_data(data_FD  = raw_FD(),
                                                                                              inf      = input$intermediate_event)),
                                                                          3)),
                                      Plot_cox_2   = forest_cox(model_cox(data_cox(trans_data(data_FD  = raw_FD(),
                                                                                              inf      = input$intermediate_event)),
                                                                          4)),
                                      circular     = circular_plot(data_FD = raw_FD()))
      )
    }
  )
  
  
  
}