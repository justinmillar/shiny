library(shiny)
library(tidyverse)
library(lubridate)
library(gridExtra)

dat <- read_csv("data/full-data.csv") %>% 
  select(Site, Date, Salinity, Temperature, Conductivity) %>%
  separate(Date, c("Date", "Time"), sep = " ") %>% 
  mutate(Date = ymd(Date))

ui <- fluidPage(
  
  titlePanel("WQ Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Site", 
                  choices=unique(dat$Site)),
      selectInput("site2", "Comparison Site", 
                  choices=c("None" = 0,unique(dat$Site))),
      dateRangeInput("date",
                     label = 'Date range input: yyyy-mm-dd',
                     start = "2017-08-01" , end = Sys.Date() + 14),
      checkboxGroupInput("variable",
                         label = h3("Observation Variable"),
                         choices = list("Salinity (ppt)" = "Salinity",
                                        "Conductivity (mS/cm)"= "Conductivity",
                                        "Temperature (C)" = "Temperature"),
                         selected = c("Salinity", "Temperature"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tables", dataTableOutput("table")),
        tabPanel("Plots", 
                 # plotOutput("salPlot"),
                 # plotOutput("conPlot"),
                 # plotOutput("tempPlot"),
                 plotOutput("plot"))
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  
  output$table <- renderDataTable({
    dat %>% 
      filter(Site == input$site,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
      select(Site, Date, Time, input$variable)
  })
  
  output$plot <-renderPlot({
    
    sal <- if ("Salinity" %in% input$variable) {
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Salinity)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Salinity)) +
          geom_point() 
      }  
    }
    
    con <- if ("Salinity" %in% input$variable) {
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Conductivity)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Conductivity)) +
          geom_point() 
      }  
    }
    
    tmp <- if ("Salinity" %in% input$variable) {
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Temperature)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Temperature)) +
          geom_point() 
      }  
    }
  
    
  })
  
  output$salPlot <- renderPlot({
    if ("Salinity" %in% input$variable) {
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Salinity)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Salinity)) +
          geom_point() 
      }  
    } else {"404"}
  })

  output$conPlot <- renderPlot({
    if ("Conductivity" %in% input$variable){
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Conductivity)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Conductivity)) +
          geom_point() 
      }
    }
  }) 
  
  output$tempPlot <- renderPlot({
    if ("Temperature" %in% input$variable) {
      if (input$site2 != 0) {
        df <- dat %>%
          filter(Site == input$site | Site == input$site2,
                 Date >= input$date[1] & Date <= input$date[2]) 
        ggplot(df, aes(x = Date, y = Temperature)) +
          geom_point() +
          facet_wrap(~ Site, ncol = 2)
      } else {
        df <- dat %>%
          filter(Site == input$site,
                 Date >= input$date[1] & Date <= input$date[2])
        ggplot(df, aes(x = Date, y = Temperature)) +
          geom_point() 
      }
    }
  }) 
  
})

# Run the application 
shinyApp(ui = ui, server = server)
