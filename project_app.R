# load data:

n_bike_station = read.csv("n_bike_station.csv")


library(lubridate)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(shiny)


SF_Map = qmap("San Franciso city", zoom=12)
SF_Map


SF_Map+
  geom_point(data=start_bike_station,aes(x=start_station_longitude,y=start_station_latitude),size=1,color="black")+
  geom_point(data=end_bike_station,aes(x=end_station_longitude,y=end_station_latitude),size=1,color="black")+
  geom_point(data=n_bike_station,aes(x=end_station_longitude,y=end_station_latitude),size=1,color="red")


ui <- fluidPage(
  titlePanel(title="Maintenance Priority",
             windowTitle = "Maintenance Priority"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose priority for maintenance"),
      selectInput(inputId = "select",
                  label = "Choose a priority",
                  choices=c("High",
                            "Medium",
                            "Low"),
                  selected = "High")   ##Notes: Prioritize the initial image
      
    ),
    mainPanel(
      #tableOutput(outputId = "table")
      plotOutput(outputId = "map")
    )
  )
)


server <- function(input, output) {
  #output$textout = renderText(input$select)
  #output$table = 
  
  output$map = renderPlot({


    # define the data as a reactive object
    # reactive can be defined at the renderPlot level
    maintain_stations = reactive({
      n_bike_station = read.csv("n_bike_station.csv")   
    })

    
    # use switch to allow map update based on what selected
    priority_selected = switch(input$select,
                               "High" = "High",
                               "Medium" = "Medium",
                               "Low" = "Low")
    
    priority_color = switch(input$select,
                               "High" = "Red",
                               "Medium" = "Yellow",
                               "Low" = "Blue")
  
    title_select = switch(input$select,
                          "High" = "High-Priority Stations",
                          "Medium" = "Medium-Priority Stations",
                          "Low" = "Low-Priority Stations")  

    
    #SF_Map = qmap("San Franciso city", zoom=12)
    #SF_Map 
    
    # plot the map
    maintain_stations = n_bike_station %>%
          filter(Priority == priority_selected) 
    
    SF_Map +
      geom_point(data=maintain_stations,
                 aes(x = end_station_longitude,
                 y = end_station_latitude),
                 size= 1.5,
                 color = priority_color) +
      ggtitle(title_select)

      
  })
}

## run the shiny app
shinyApp(ui = ui, server = server)
