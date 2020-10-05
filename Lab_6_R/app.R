#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(lubridate)
library(viridis)
library(ggplot2)

time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed") 

time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths")

time_series_recovered_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region") %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Recovered")

time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)

time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Deaths)

time_series_recovered_long <- time_series_recovered_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Recovered)

# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))

time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
    select(-Key)

# Reformat the data
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)

time_series_long_joined_counts <- time_series_long_joined %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date),
                 names_to = "Report_Type", values_to = "Counts")

type = c("Confirmed", "Deaths", "Recovered")
country = c("US", "India", "Brazil", "Colombia", "Peru")

time_series_confirmed_US = read_csv("time_series_covid19_confirmed_US.csv")

state = c('Massachusetts', "New York", "California")


#####################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Combining R Shiny Apps & Covid-19 Data"),
    tags$br(),
    tags$hr(),
    
    # Sidebar with a slider input for date
    sidebarLayout(
        sidebarPanel(
            sliderInput("date",
                        label = 'Date:',
                        min = as.Date("2020-01-22","%Y-%m-%d"),
                        max = as.Date("2020-09-28","%Y-%m-%d"),
                        value=as.Date("2020-01-22"),timeFormat="%Y-%m-%d")
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("timePlot")
        )
    ), 
    tags$hr(),
    
    # Sidebar with a select inputs to choose variables to plot
    sidebarLayout(
        sidebarPanel(
            selectInput("y_variable", "Report Type", type, selected = "Confirmed")
        ),
        
        # Show a plot with configurable axes
        mainPanel(
            plotOutput("casePlot")
        )
    ),
    tags$hr(),

    sidebarLayout(
        sidebarPanel(
            selectInput("report", "Report Type", type, selected = "Confirmed"),
            selectInput("country", "Country", country, selected = "US")
            
        ),
        
        # Show a plot with configurable axes
        mainPanel(
            plotOutput("caseCountryPlot")
        )
    ),
    
    tags$hr(),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$timePlot <- renderPlot({
        reportType = time_series_long_joined_counts%>%
            filter (Country_Region %in% c("US","India","Brazil","Russia", "Colombia", "Peru"))%>%
            filter (Report_Type == "Deaths")
        
        ggplot(reportType, mapping = aes(x = reportType$Date, y = reportType$Counts, color = reportType$Country_Region)) +
            geom_point(size=2) +
            xlim(as.Date("2020-01-22","%Y-%m-%d"), input$date)+
            theme_light()+
            ylab("Count")+
            xlab("Date")+
            ggtitle("Time-line of Confirmed Cases of Covid-19 Across Countries with Highest Infection Rate")
    })
    
    output$casePlot <- renderPlot({
        reportType1 = time_series_long_joined_counts[ which(time_series_long_joined_counts$Report_Type == input$y_variable),]
        reportType1 = reportType1%>%
            filter (Country_Region %in% c("US","India","Brazil","Russia", "Colombia", "Peru"))
        ggplot(reportType1, mapping = aes(x = reportType1$Date, y = reportType1$Counts, color = reportType1$Country_Region))+
            geom_point(size=2) +
            theme_light()+
            ylab("Count")+
            xlab("Date")+
            ggtitle("Time-line of Covid-19 Cases by Report Type for Most Infected Countries")
    })
    
    output$caseCountryPlot <- renderPlot({
        
        time_fin2 = time_series_long_joined_counts%>%
            filter(Country_Region == input$country)%>%
            filter(Report_Type == input$report)
        
        ggplot(time_fin2, mapping = aes_string(x = "Date", y = "Counts"))+
            geom_point(size=2) +
            theme_light()+
            ylab("Count")+
            xlab("Date")+
            ggtitle("Covid 19 Case Report Type by Country")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
