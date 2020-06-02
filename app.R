library(shiny)
library(shinythemes)
library(plotly)
source('./app_utils.R')

data <- load_data()
pop_data <- load_pop_data()

ui <- fluidPage(
    theme = shinytheme('simplex'),
    tabsetPanel(
        tabPanel("Over Time",
            sidebarLayout(
             sidebarPanel(
                 selectInput(
                     'time_unit',
                     'Time Period',
                     choices = c('day','month','year'),
                     selected = 'month'
                 ),
                 
                 selectInput(
                     'plot_by',
                     'By',
                     choices = c('none','race','gender'),
                     selected = 'none'
                 ),
                 
                 dateRangeInput(
                     'date_range',
                     'Date Range',
                     start = min(data$date),
                     end = max(data$date),
                     min = min(data$date),
                     max = max(data$date)
                 )
             ),
             
             mainPanel(
                 plotlyOutput('n_over_time_plot')
             )
            )  
        ),
        tabPanel("Proportions",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        'plot_props_by',
                        'By',
                        choices = c('race','gender'),
                        selected = 'race'
                    )
                ),
                mainPanel(
                    plotOutput('props_plot'),
                    textOutput('props_text')
                )
            )
        )
    )
    
)

server <- function(input, output) {
    output$n_over_time_plot <- renderPlotly({
        min_date <- as.Date(
            input$date_range[1],
            format = '%Y-%m-%d'
        )
        max_date <- as.Date(
            input$date_range[2],
            format = '%Y-%m-%d'
        )
        
        data %>% plot_over_time(
            input$time_unit, by = input$plot_by, min_date = min_date, max_date = max_date
        )
    })
    
    output$props_plot <- renderPlot(
        data %>% plot_props(by = input$plot_props_by)
    )
    
    output$props_text <- renderText(
        data %>% write_props(by = input$plot_props_by)
    )
}

shinyApp(ui = ui, server = server)
