#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(tidyr)

# get the data
mortality <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE)

# Define UI for application that draws a histogram
ui <- navbarPage("Mod3",
        tabPanel(
            "Q1: 2010 Mortality by Cause in the United States",
            selectizeInput(
                inputId = "Cause",
                label = "Select Cause of Death:",
                choices = unique(m_2010$ICD.Chapter),
                selected = "Diseases of the digestive system",
                multiple = FALSE
                ),
                plotlyOutput(outputId = "a", height = "800px", width = "800px"),
                helpText("Data source: CDC WONDER system, https://wonder.cdc.gov/ucd-icd10.html"),
            ),
        tabPanel(
            "Q2: 2010 Mortality by State in the United States",
            fluidRow(
                column(12, 
                       selectizeInput(
                           inputId = "State", 
                           label = "Select State:", 
                           choices = unique(m_2010$State), 
                           selected = "IN", 
                           multiple = F),
                       selectizeInput(
                           inputId = "Cause", 
                           label = "Select Cause of Death:", 
                           choices = unique(m_2010$ICD.Chapter), 
                           selected = "Diseases of the digestive system", 
                           multiple = F)
                ),
                fluidRow(
                    column(6, 
                           plotlyOutput(outputId = "b", 
                                        height = "600px", 
                                        width = "1200px"))
                ),
                helpText("Data source: CDC WONDER system, https://wonder.cdc.gov/ucd-icd10.html"),
            )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$a <- renderPlotly({
        m_2010 <- mortality %>%
            filter(Year == "2010") %>%
            group_by(State, ICD.Chapter) %>%
            summarise(deaths = sum(Deaths),
                      p = sum(Population), 
                      crude = deaths / p * 100000) %>%
            filter(ICD.Chapter == input$Cause) %>%
            arrange(desc(crude)) %>%
            plot_ly(.,
                    x = ~crude,
                    y = ~reorder(State, crude),
                    textposition = 'auto',
                    marker = list(color = 'rgb(66,245,197)',
                                  line = list(color = 'rgb(2,100,54)'))) %>%
            layout(title = "Mortality Rate by State",
                   xaxis = list(title = "Crude Rate per 100K"),
                   yaxis = list(title = "State"))
    })

    # Define server logic required to draw a line plot    
    output$b <- renderPlotly({
    mortality %>%
        group_by(Year,ICD.Chapter) %>%
        mutate(National_Avg = as.numeric(mean(Crude.Rate)),
               Delta = Crude.Rate - National_Avg,
               Year = as.Date(ISOdate(Year, 12, 31))) %>%
        filter(State == input$State & ICD.Chapter == input$Cause) %>%
        ungroup() %>%
        plot_ly(., x = ~Year, y = ~Crude.Rate, type = 'scatter', mode = 'lines', name = 'State') %>%
        add_trace(y = ~National_Avg, name = 'National Avg', connectgaps = T) %>%
        layout(title = "Change in Mortality Rate by Year",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Mortality Rate per 100K"))
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
