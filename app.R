library(shiny)
library(shinythemes)
library(plotly)
library(readxl)
library(janitor)
library(tidyverse)
library(lubridate)
library(grid)
library(leaflet)
library(geojsonio)
library(shinyWidgets)

source('./app_utils.R')

data <- load_data()
pop_data <- load_pop_data()

ui <- fluidPage(
    theme = shinytheme('simplex'),
    titlePanel("Fatal Encounters"),
    tabsetPanel(
        tabPanel(
            "Plots Over Time",
            br(),
            fluidRow(
                column(
                    3,
                    selectInput(
                        'time_unit',
                        'Time Period',
                        choices = c('year','month','day'),
                        selected = 'year'
                    )
                ),
                
                column(
                    3,
                    selectInput(
                        'plot_by',
                        'By',
                        choices = c('race','gender','none'),
                        selected = 'race'
                    )
                ),
                
                column(
                    4,
                    dateRangeInput(
                        'date_range',
                        'Date Range',
                        start = min(data$date),
                        end = as.Date('12-31-2019', format = '%m-%d-%Y'),
                        min = min(data$date),
                        max = max(data$date)
                    )
                )
            ),

            plotlyOutput('n_over_time_plot'),
            textOutput('text_over_time')
        ),
        tabPanel(
            "Distributions",
            br(),
            
            radioGroupButtons(
                inputId = "plot_props_by",
                choices = c("race", "gender"),
                selected = "race",
                checkIcon = list(
                    yes = icon("ok", lib = "glyphicon")
                )
            ),
            
            plotOutput('props_plot'),
            textOutput('props_text')
        ),
        
        tabPanel("Across States",
            br(),
            sliderInput(
                "year_in",
                "Year",
                min = min(data$date_year, na.rm = T),
                max = max(data$date_year, na.rm = T),
                value = min(data$date_year, na.rm = T),
                animate = animationOptions(
                    playButton = icon('play', 'fa-2x'),
                    pauseButton = icon('pause', 'fa-2x')
                )
            ),
            h3(paste("Annual Deaths per State")),
            leafletOutput('map_plot'),
            h5("Cumulative US Deaths over Time"),
            plotOutput('line_plot', height = '100px')
        ),
        tabPanel(
            "About",
            
            br(),
            
            h3('Fatal Encounters Data'),
            
            p(HTML(paste("The", a(href = "https://fatalencounters.org/", "Fatal Encounters"),
                         "project was founded on the premise that Americans should have the ability",
                         "to track circumstances where police use deadly force, even though these circumstances",
                         "are not tracked by any governmental organization. This project, run by D. Brian",
                         "Burghart, has built a database of all deaths through police interaction in the United States",
                         "since January 1, 2000."
            ))),
            
            p(HTML(paste(
                "This Shiny Application provides a simple, interactive format for exploring this dataset.",
                "Fatal Encounters has done a fabulous job gathering this data, keeping it publically available,",
                "and creating a simple search engine for specific cases. The purpose of this application is to",
                "make the information from the database more accessible to the general public, not just data",
                "scientists and statisticians."
            ))),
            
            p(HTML(paste(
                "Before using this data for my visualizations, I followed D. Brian Burghart's advice",
                "and filtered out suicides, medical emergencies, drug overdoses, and criminal acts.",
                "In the event of a suicide, emergency, or overdose, the death was not at",
                "the hands of a police officer. Burghart says that at this point in their data collection,",
                "this filtering is the best way to determine cases of intentional use of force."
            ))),
            
            h3('Other Data Soruces'),
            
            p(HTML(paste(
                "Estimated proportions of the US population that are in each racial and gender group ",
                "were found on the ", 
                a(href = "https://www.census.gov/quickfacts/fact/table/US/IPE120218", "Census QuickFacts Page"), ". ", 
                "Transgender individuals are not counted as such in the United States Census, ",
                "so this proportion was found separately from the ",
                a(href =  "https://en.wikipedia.org/wiki/LGBT_demographics_of_the_United_States#:~:text=A%20different%20survey%20in%202016,adult%20population%20identifying%20as%20LGBT", 
                  "Wikipedia page on LGBT demographics of the United States"
                ),
                ".", sep = ''
            )))
        )
    )
)

server <- function(input, output, session) {
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
            pop_data, input$time_unit, by = input$plot_by, min_date = min_date, max_date = max_date
        )
    })
    
    br()
    
    output$text_over_time <- renderText({
        write_over_time(data, input$plot_by)
    })
    
    output$props_plot <- renderPlot(
        data %>% plot_props(by = input$plot_props_by)
    )
    
    output$props_text <- renderText(
        data %>% write_props(by = input$plot_props_by)
    )
    
    observeEvent('year_in', {
        observe({
            out <- plot_states(data, pop_data, input$year_in)
            output$map_plot <- renderLeaflet(out[['map']])
            output$line_plot <- renderPlot(out[['line']])
        })
    })
}

shinyApp(ui = ui, server = server)
