library(dplyr)
source("machine_learning.R")
source("feature_importance.R")

library(shiny)
ui <- fluidPage(
  titlePanel(
    "Make ML reproducible again"
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload a file", accept = c(".csv", ".tsv")),
      numericInput("y_pos", "Your output label column", value = 2, min = 1, max = 20000),
      numericInput("x_start", "Start column of features", value = 8, min = 1, max = 20000),
      numericInput("x_end", "End column of features (0 = last column)", value = 0, min = 0, max = 20000),
      actionButton("select_columns", "Select columns!"),
      tableOutput("performanceTable"),
      textInput("best_model", "Select the best model (performance vs. inferences)"),
      actionButton("get_feature_importance", "Post analysis (feature importance)")
      #actionButton("fit", "Run ML screening!"),
      #actionButton("plot_fits", "Plot fits!")
      # inputs
    ),
    mainPanel(
      # tableOutput("table"),
      tableOutput("summary"),
      tableOutput("used_data"),
      plotOutput("performance"),
      plotOutput("heatmap") #,plotOutput("correlations")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2) # 100 mb

  dataset <- reactive({
    
    file <- input$upload$name
    
    if (is.null(input$upload)){return()}
    if (grepl("[.]csv", file)){
      return(readr::read_csv(input$upload$datapath))
    } else {
      return(readr::read_tsv(input$upload$datapath))
    }
  })
  
  preprocessed <- eventReactive(
    input$select_columns, {
      df <- dataset()
      y  <- df[[input$y_pos]]
      x_end <- ifelse(input$x_end == 0, ncol(df), input$x_end)
      x <- df[input$x_start:x_end]
      x <- as.matrix(x)^1/4
      x <- pqn(x)
      return(list("x"=x, "y"=y))
  })
  
  fits <- eventReactive(
    input$select_columns,
    {
      df <- preprocessed()
      print(df)
      fit <- fit_models(x=df$x, y=df$y)
      return(fit)
      })
  
  observeEvent(
    input$select_columns, 
    output$performance <- renderPlot({
      fit <- fits()
      plot_performance(fit)
      },
      width = 800))
  
  observeEvent(
    input$get_feature_importance,
    output$heatmap <- renderPlot({
      fit <- fits()
      x <- preprocessed()$x
      features <- feature_selection(input$best_model, fit)
      heatmap_pathway(x, features)
    },
    width = 400)
  )
  
  # observeEvent(
  #   input$get_feature_importance,
  #   output$correlations <- renderPlot({
  #     fit <- fits()
  #     x <- preprocessed()$x
  #     features <- feature_selection(input$best_model, fit)
  #     correlation_map(x, features)
  #   })
  # )
  
  
  output$summary <- renderTable({
      head(dataset()[1:10])
    })
  
  output$performanceTable <- renderTable(
    get_performance(fits())
  )
}

shinyApp(ui, server)
