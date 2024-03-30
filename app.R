# Shiny app for 

## To do:
### Add more models
### Make plots pretty

sapply("./von_bertalanffi.R",
       FUN = source
)

library(shiny)
library(bslib)
library(data.table)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)

my_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "darkgray",
  base_font = "Helvetica Neue"
)

years = c(0:100)  # A vector with time
rmse <- function(actual, predicted){
  return(sqrt(mean((actual - predicted)^2, na.rm = TRUE)))}
sse <- function(actual, predicted){
  return(sum((predicted - actual)^2, na.rm = TRUE))}

ui <- fluidPage(
  theme = my_theme,
  titlePanel("Fit a model to AGB data"),
  sidebarLayout(
  # Define loayout:
    # A side bar pane with a prompt to input a text file:
    sidebarPanel(
      fileInput("file", 
                "Select a file"),
      selectInput("model", 
                  "Select a model", 
                  choices = c("Linear", "Exponential", "Von Bertalanffy")),
      uiOutput('vonBertalanffi_parameters1'),
      uiOutput('vonBertalanffi_parameters2'),
      downloadButton('download',"Download Results")
    ),
    # The output will go in the main panel:
    mainPanel(
      tabsetPanel(
        tabPanel("Raw data plot", plotOutput("plot_input")),
        tabPanel("Summary of model output", plotOutput("model_plot"),
                 tableOutput("model_eval")),
        tabPanel("Full model output", tableOutput("model_fit_table")),
        tabPanel("Model summary", textOutput("model_fit_summ"))
    )),
    position = c("left", "right"),
    fluid = TRUE
  )
)

server <- function(input, output, session) {
  # Create a reactive expression to read the text file:
  input_file <- reactive({
    # If not file, then don't return anything:
    if (is.null(input$file)) {
      return("")
    }
    # Actually read the file
    raw_data <- read.csv(file = input$file$datapath)
    agb_data <- data.frame(Age = raw_data$Age, 
               TAGB = 0.45*(raw_data$AGB + raw_data$Palm + raw_data$HV))
  })
  
  output$vonBertalanffi_parameters1 <- renderUI({
    if(input$model == "Von Bertalanffy"){
      tagList(
        numericInput('ymax', "Ymax", value = 300),
        checkboxInput('vb_b1_b2', "Do you want to input starting values for B0, B1 and B2?", value = FALSE)
      )
    }
  })
  
  output$vonBertalanffi_parameters2 <- renderUI({
    if(input$model == "Von Bertalanffy"){
      if(input$vb_b1_b2 == FALSE){
        return()
        }
      else(
        tagList(
          numericInput('b1', "B1", value = 0.009),
          numericInput('b2', "B2", value = 0.5)
          )
      )
    }
  })
  
  # Disply output to plot with ID "plot_input":
  output$plot_input <- shiny::renderPlot({    # Reactives are only callable inside a reactive context like render
    # Render only if there is data available:
    req(input_file())
    # Acually read the file:
    data <- input_file()
    data_plot <- ggplot(data, aes(Age, TAGB)) +
      geom_point() +
      theme_minimal()
    data_plot
  })
  
  model_fit <- reactive({
    req(input_file())
    data <- input_file()
    ymax_vonB <- input$ymax
    b1_vonB <- input$b1
    b2_vonB <- input$b2
    
    if(input$model == "Linear") {
      # Fit linear model to input_file
      linear_model_out = lm(data$TAGB ~ data$Age)
      linear_f <- function(t){
        coef(linear_model_out)[1] + coef(linear_model_out)[2] * t
      }
      # Run the function to get predicted AGB:
      linear_pred = linear_f(years) # Predicted AGB
      data_wpred <- data.frame(Age = years,
                                TAGB_pred = linear_pred) %>%
        left_join(data, by = "Age")
      return(data_wpred)
    }
    
    else if(input$model == "Exponential"){
      # Fit linear model to input_file
      exp_model_out = nls(data$TAGB ~ a * exp(r * data$Age), start = list(a = 0.5, r = 0.5))
      exp_f <- function(t){
        coef(exp_model_out)[1] * exp(coef(exp_model_out)[2] * t)
      }
      # Run the function to get predicted AGB:
      exp_pred = exp_f(years) # Predicted AGB
      data_wpred <- data.frame(Age = years,
                               TAGB_pred = exp_pred) %>%
        left_join(data, by = "Age")
      return(data_wpred)
    }
      
    else if (input$model == "Von Bertalanffy") {
      if(input$vb_b1_b2 == FALSE){    # if no starting parameters were provided for b1 and b2, run with defaults
        # Fit the Von Bertalanffy model to input_file
        #vonB_model <- nls(TAGB~(ymax_vonB*0.45)*(1-exp(-b1*Age))^b2, data = data, start=list(b1=0.05,b2=1))
        vonB_model <- fit_vonbert_model(Age = data$Age, AGB = data$TAGB, 
                                        ymax = ymax_vonB)
        }
      else if(input$vb_b1_b2 == TRUE){    # if starting parameters were provided for b1 and b2, use them
        vonB_model <- fit_vonbert_model(Age = data$Age, AGB = data$TAGB,
                                       ymax = ymax_vonB,
                                       b1_start = b1_vonB, b2_start = b2_vonB)
        }
      # Run the function to get predicted AGB:
      vonB_pred <- vonbert_model_f(years,
                                   ymax = ymax_vonB,
                                   b1 = coef(vonB_model)[1], b2 = coef(vonB_model)[2]) # Predicted AGB
      data_wpred <- data.frame(Age = years,
                               TAGB_pred = vonB_pred) %>%
        left_join(data, by = "Age")
      return(data_wpred)
    }
  })
  
  output$model_fit_summ <- shiny::renderPrint({
    req(model_fit())
    model_summ <- model_fit()
    return(summary(model_summ))
  })
  
  output$model_fit_table <- shiny::renderTable({
    req(model_fit())
    model_data <- model_fit()
    return(model_data)
    })

  output$model_plot <- shiny::renderPlot({
    req(model_fit())
    model_data <- model_fit()
    # Plot the modeled AGB:
    plot(model_data$Age, model_data$TAGB,
         xlim=c(0,100),
         ylim=c(0,200),
         xlab="Successional age (years)",
         ylab=expression(paste("TAGB (Mg C ",ha^-1,")")))
    lines(model_data$Age, model_data$TAGB_pred)
    })
  
  output$model_eval <- shiny::renderTable({
    req(model_fit())
    model_data <- model_fit()
    lm_pred <- lm(TAGB_pred ~ TAGB, data = model_data)
    pred_r_squared <- summary(lm_pred)$r.squared
    pred_rmse <- rmse(model_data$TAGB, model_data$TAGB_pred)
    pred_sse <- sse(model_data$TAGB, model_data$TAGB_pred)
    data.frame(r2 = pred_r_squared, 
               rmse = pred_rmse,
               sse = pred_sse)
  })
  
  output$download <- downloadHandler(
    filename = function(){"model_output.csv"}, 
    content = function(fname){
      write.csv(model_fit(), fname, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)



# Archive:

## UI:
# Show the input data as a table (ID "table"):
#DT::dataTableOutput("table")

## Server:
# Format output to table with ID "table":
# output$table <- DT::renderDataTable({    #Reactives are only callable inside a reactive context like render
#   # Render only if there is data available:
#   req(input_file())
#   # Acually read the file:
#   data <- input_file()
#   data_summary <- summary(data)
#   data_summary
# })
