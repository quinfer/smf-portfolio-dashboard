library(dplyr)
library(dygraphs)
library(plotly)
library(PerformanceAnalytics)
library(shiny)
library(shinydashboard)
library(xts)
library(zoo)
library(tidyverse)
MyDeleteItems<-ls()
rm(list=MyDeleteItems)
returns<-readRDS("returns.rds")


# smf_ftse %>% select(-Date) %>%
#   xts(order.by = smf_ftse$Date)
returns[['Full']] %>% select(-Sector,-Date) %>% drop_na() %>% xts::xts(order.by = returns[['Full']]$Date)->rtns

rtns %>% rollapply(250,width=200,
                   function(df) InformationRatio())



portfolio_choices <- c(
  "Full Portfolio" = "Full",
  "Materials" = "Materials",
  "Financials" = "Financials",
  "Health Care" = "Health Care",
  "Real Estate" = "Real Estate",
  "Communication Services" = 'Communication Services',
  "Consumer Discretionary" = "Consumer Discretionary",
  "Consumer Staples" = "Consumer Staples",
  "Industrials" = "Industrials",
  "Information Technology" = "Information Technology",
  "Macro",
  "Utilities"= "Utilities",
  "Energy"
)

ui <- dashboardPage(
  dashboardHeader(title = "QMSF Portfolio Dashboard"),
  dashboardSidebar(
    selectInput(
      "portfolio",
      "Choose a portfolio",
      choices = portfolio_choices,
      selected = "smf_ftse"
    ),
    dateInput(
      inputId = "date",
      label = "Starting Date",
      value = "2016-05-02",
      format = "yyyy-mm-dd"
    ),
    sliderInput("mar", "Min Acceptable Rate", min = 0, max = 0.1, value = 0.008, step = 0.001),
    numericInput("window", "Rolling Window", min = 6, max = 36, value = 12)
  ),
  dashboardBody(
    fluidRow(
      box(title = "Relative Price Performance", width = 12,
        plotlyOutput("time_series_PR")
      )
    ),
    fluidRow(
      box(title = "Information Ratio", width = 4,
        plotlyOutput("time_series_IR", height = 250)
      ),
      box(title = "Sortino Ratio", width = 4,
        plotlyOutput("time_series_SR", height = 250)
      ),
      box(title = "Holdings Information", width = 4,
        plotlyOutput("table", height = 250)
      )
    )
  )
)

server <- function(input, output) {
  
  rate_limit_sec <- 2
  
  portfolio_selected <- throttle(reactive({
    req(input$portfolio, input$date)
    
    returns[[input$portfolio]] %>%
      # as_tibble() %>%
      #collect() %>%
      # mutate(date = as.Date(date)) %>%
      filter(date >= input$date)
    
  }), rate_limit_sec * 1000)
  
  rolling_sortino <- reactive({
    req(input$mar)
    req(input$window)
    
    portfolio_selected()$pr_rtns %>%
      xts::xts(order.by = portfolio_selected()$Date) %>%
      rollapply(input$window, function(x) SortinoRatio(x, MAR = input$mar)) %>%
      `colnames<-`("24-rolling")
  })

  rolling_info_ratio <- reactive({
    
    portfolio_selected() %>%
      xts::xts(order.by = portfolio_selected()$Date) %>%
      
  })
  
  
    
  output$time_series <- renderPlotly({
    plot_ly() %>%
      add_lines(x = index(rolling_sortino()), y = as.numeric(rolling_sortino())) %>%
      layout(
        hovermode = "x",
        xaxis = list(
          rangeslider = list(visible = TRUE),
          rangeselector = list(
            x = 0, y = 1, xanchor = 'left', yanchor = "top", font = list(size = 9),
            buttons = list(
              list(count = 1, label = 'RESET', step = 'all'),
              list(count = 1, label = '1 YR', step = 'year', stepmode = 'backward'),
              list(count = 3, label = '3 MO', step = 'month', stepmode = 'backward'),
              list(count = 1, label = '1 MO', step = 'month', stepmode = 'backward')
            )        
          )
        )
      )
  })
  
  output$scatterplot <- renderPlotly({
    portfolio_scatter <- ggplot(sortino_byhand(), aes(x = date, y = returns, color = status) )+
      geom_point() +
      geom_vline(xintercept = as.numeric(as.Date("2016-11-30")), color = "blue") +
      geom_hline(yintercept = input$mar, color = "purple", linetype = "dotted") +
      scale_color_manual(values = c("tomato", "chartreuse3")) +
      theme(legend.position = "none") + ylab("percent monthly returns")
    
    ggplotly(portfolio_scatter) %>% 
      add_annotations(
        text = "Trump", x = as.numeric(as.Date("2016-11-30")), 
        y = -.05, xshift = -10, textangle = -90, showarrow = FALSE
      )
  })
  
  output$histogram <- renderPlotly({
    p <- ggplot(sortino_byhand(), aes(x = returns)) +
      geom_histogram(alpha = 0.25, binwidth = .01, fill = "cornflowerblue") +
      geom_vline(xintercept = input$mar, color = "green")
    ggplotly(p) %>%
      add_annotations(text = "MAR", x = input$mar, y = 10, xshift = 10, showarrow = FALSE, textangle = -90)
  })
  
  output$density <- renderPlotly({
    sortino_density_plot <- ggplot(sortino_byhand(), aes(x = returns)) +
      stat_density(geom = "line", size = 1, color = "cornflowerblue")
    
    shaded_area_data <- ggplot_build(sortino_density_plot)$data[[1]] %>%
      filter(x < input$mar)
    
    sortino_density_plot <-
      sortino_density_plot +
      geom_area(data = shaded_area_data, aes(x = x, y = y), fill = "pink", alpha = 0.5) +
      geom_segment(
        data = shaded_area_data, aes(x = input$mar, y = 0, xend = input$mar, yend = y),
        color = "red", linetype = "dotted"
      )
    
    ggplotly(sortino_density_plot) %>%
      add_annotations(
        x = input$mar, y = 5, text = paste("MAR =", input$mar, sep = ""), textangle = -90
      ) %>%
      add_annotations(
        x = (input$mar - .02), y = .1, text = "Downside", 
        xshift = -20, yshift = 10, showarrow = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)
