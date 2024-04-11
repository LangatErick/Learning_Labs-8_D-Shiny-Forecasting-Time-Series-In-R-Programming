#importing packages & Setup
library(shiny)
library(feasts)
library(pacman)
p_load(
  tidyr,
  haven,
  fable,
)
library(fpp3)
library(tsibble)
library(readr)
library(tidyverse)
library(plotly)
library(magrittr)
library(DT)
library(rsconnect)
############################################
#Loading data
###################################
Flour <- as_tsibble(read_csv("APU0000701111_Flour.csv"), index=DATE)
Bread <- as_tsibble(read_csv("APU0000702111_Bread.csv"), index=DATE)
Cookies <- as_tsibble(read_csv("APU0000702421_Cookies.csv"), index=DATE)

Flour %<>% mutate(
  DATE= yearmonth(DATE),
  price = as.numeric(APU0000701111))

Cookies %<>% mutate(
  DATE= yearmonth(DATE),
  price = APU0000702421)

Bread %<>% mutate(
  DATE= yearmonth(DATE),
  price = APU0000702111)

ui <- shiny::fluidPage(
  
  # Application title
  titlePanel("Forecasting The Prices of The Consumer Commondities"),
  
  br(),
  
  h5("Author: Langat Erick"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("dataset", "Dataset:",
                  list("Chocolate Chip Cookies" = "Cookies", 
                       "Bread" = "Bread",
                       "Flour" = "Flour")),
      numericInput("ahead", "Years to Forecast Ahead:", 3),
      
      submitButton("Update Your Forecast Horizon")
    ),
    
    
    # Show the caption and forecast plots
    mainPanel(
      h3(textOutput("caption")),
      
      tabsetPanel(
        tabPanel("Forecast", plotOutput("ForecastPlot")), 
        tabPanel("Timeseries Decomposition", plotOutput("dcompPlot")),
        tabPanel("Model Selection", DT::DTOutput("selection")),
        tabPanel("Residual Diagnostics", plotOutput("residuals"))
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  getDataset <- reactive({
    if (input$dataset=="Cookies")
    {
      return(Cookies)
    }
    else if (input$dataset=="Flour")
    {
      return(Flour)
    }
    else
    {
      return(Bread)
    }
    
  })
  
  output$caption <- renderText({
    paste("Dataset: ", input$dataset)
  })
  
  output$dcompPlot <- renderPlot({
    ds_ts <- STL(getDataset(), price ~ season(window = Inf))
    autoplot(ds_ts)
  })
  
  output$ForecastPlot <- renderPlot({
    
    getDataset() %>%
      model(ETS= ETS(price),
            ARIMA = ARIMA(price)) %>%
      forecast(h = paste0(input$ahead," years")) %>%
      autoplot(getDataset()) +
      labs(title = paste0(input$ahead, " year forecasts for price of ", input$dataset),
           y="Price",
           x="Date") +
      theme_light()
  })
  
  output$selection <- DT::renderDataTable({
    
    fit <- getDataset() %>%
      filter(DATE < yearmonth("2014 Dec")) %>%
      model(
        ETS(price),
        ARIMA(price)) %>%
      forecast(h= "7 years")
    
    selection <- fit %>% accuracy(getDataset())
    
    selection
    
  })
  
  output$residuals <- renderPlot({
    getDataset() %>%
      model(ETS(price)) %>%
      gg_tsresiduals() +
      labs(title = "Diagnostics")
    
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)