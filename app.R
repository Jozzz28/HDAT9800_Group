#
# HDAT 9800 Group Assessment — Health Explorer
#
# Group member task 1:  Avinash Sharma
# Group member task 2:  Tanya Land
# Group member task 3:  Jo Zhang
# Group member task 4:  Damian O'Neil

library(shiny)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(tidyverse)
library(janitor)
library(rgeos)
library(leaflegend)
library(stringr)
library(htmltools)

# Task 1
#Loading data

state_hospitals <- read.csv("data/hospitals.csv")

#Cleaning data names 
hospitals.t1 <- clean_names(state_hospitals)

hospitals.t1$state <- str_replace(hospitals.t1$state, "Qld", "QLD")
hospitals.t1$beds <- replace(hospitals.t1$beds, is.na(hospitals.t1$beds), "Others")
hospitals.t1$description <-replace(hospitals.t1$description, is.na(hospitals.t1$description), "Not available")

# Icon list
HospitalIcons <- awesomeIconList(
  Public = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'green', iconColor = 'white', library = "fa"),
  Private = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'orange', iconColor = 'white', library = "fa")
)

# Input choices
choices_state <- c( "Australian Capital Territory" = "ACT", 
                    "Queensland"=  "QLD", "South Australia"= "SA",
                    "Northern Territory" = "NT", "New South Wales"= "NSW",
                    "Victoria" = "Vic", "Tasmania"= "Tas",
                    "Western Australia"= "WA", "All") 

choices_sector <- c(unique(hospitals.t1$sector), "All")

choices_beds <- c(unique(hospitals.t1$beds), "Any" )


#Task 2:
#read data files and shp files

unzip(zipfile = 'data/1270055001_ste_2016_aust_shape.zip')
polys_1 <- readOGR("STE_2016_AUST.shp")


#Identifying inconsistent State names in dataset
table(state_hospitals$State)

# tidy up the data
hosp1 <- state_hospitals %>%
  clean_names() %>%
  mutate(
    state =recode(state,  "Qld" = "Queensland",
                  "QLD" = "Queensland", 
                  "Tas" = "Tasmania",
                  "Vic" = "Victoria",
                  "WA" = "Western Australia",
                  "NSW" = "New South Wales",
                  "SA" = "South Australia",
                  "NT" = "Northern Territory",
                  "ACT" = "Australian Capital Territory"),
    state=factor(state), #setting as factors to make dropdown menu coding more efficient
    sector=factor(sector))

# define hospitals with emergency departments
table(hosp1$description) # all have "emergency" in description field
# create flag ('ed') for hospitals with emergency dept
state_hospitals2 <- hosp1 %>%
  mutate(ed = str_detect(description, "emergency"))
# subset to hospitals with ED only
data_ed <- filter(state_hospitals2, ed == TRUE)


# create colour palette
palette2 <- colorNumeric(palette = c("red", "orange", "yellow", "white"), 
                         domain = c(0, 1000),
                         na.color = "white")

# simplify the polygons, but warning: this step changes SP dataframe to SP file
state_polys <- rgeos::gSimplify(polys_1, tol = 0.01)
# add data back to polygons to make SP dataframe
state_polysdf <- SpatialPolygonsDataFrame(state_polys, polys_1@data)


# Hospital icons
HospitalIcons2 <- makeAwesomeIcon(icon = 'hospital-o', markerColor = 'red', iconColor = 'white', library = "fa")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    tabPanel("Task 1 — Hospital Search",  p(),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "state.t1",
                   label = "State:",
                   choices = choices_state),
                 
                 selectInput(
                   inputId = "sector.t1",
                   label = "Sector:",
                   choices = choices_sector),
                 
                 selectInput(
                   inputId = "beds.t1",
                   label = "Number of beds:",
                   choices = choices_beds)
               ),
               
               mainPanel(
                 leafletOutput(outputId = "map"),
                 uiOutput(outputId = "message")
               )
             )
             
    ),
    tabPanel("Task 2 — Distance from Emergency Department", p(),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Task2.state",
                   label = "State",
                   choices = levels(data_ed$state)
                 ),
                 numericInput(
                   inputId = "Task2.max.distance",
                   label = "Maximum distance (in km, 0-1000)",
                   value = 500,
                   min = 0,
                   max = 1000,
                   step = 25
                 ),
                 numericInput(
                   inputId = "Task2.npoints",
                   label = "Number of grid points (1000-10,000)",
                   value = 5000,
                   min = 1000,
                   max = 10000,
                   step = 1000
                 ),
                 checkboxInput(
                   inputId = "Task2.show.markers",
                   label = "Show markers",
                   value = FALSE
                 ),
                 actionButton(
                   inputId = "Task2.update",
                   label = "Update",
                   style = "color: white; background-color: blue"
                 )
               ),
               
               mainPanel(
                 leafletOutput("ed.plot")
               )
             )
             
    ),
    tabPanel("Task 3 — Length of Hospital Stay", p(),
             
             sidebarLayout(
               sidebarPanel(
                 # add task 3 sidebar elements here
               ),
               
               mainPanel(
                 # add task 3 main panel elements here
               )
             )
             
    ),
    tabPanel("Task 4 — TBC", p(),
    
             sidebarLayout(
               sidebarPanel(
                 # add task 4 sidebar elements here
                 checkboxInput("djo.test", "Test"),
               ),
               
               mainPanel(
                 # add task 4 main panel elements here
                 print("Hello World")
               )
             )
             
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Task 1
  #filtering data to a reactive function
  
  hospital_r <- reactive ({
    
    if (input$state.t1 != "All" & input$sector.t1 != "All" & input$beds.t1 !="Any" ){
      filter( hospitals.t1, state== input$state.t1, sector == input$sector.t1 , beds == input$beds.t1)}
    
    else if ( input$state.t1 == "All" & input$sector.t1 !="All" & input$beds.t1 !="Any") {
      filter(hospitals.t1, sector== input$sector.t1, beds == input$beds.t1)}
    
    else if ( input$state.t1 != "All" & input$sector.t1 =="All" & input$beds.t1 !="Any") {
      filter(hospitals.t1, state== input$state.t1, beds == input$beds.t1)}
    
    else if ( input$state.t1 != "All" & input$sector.t1 !="All" & input$beds.t1 =="Any") {
      filter(hospitals.t1, state== input$state.t1, sector == input$sector.t1)}
    
    else if (input$beds.t1 != "Any" & input$state.t1 == "All" & input$sector.t1 == "All") {
      filter(hospitals.t1, beds== input$beds.t1)}
    
    else if (input$beds.t1 == "Any" & input$state.t1 != "All" & input$sector.t1 == "All") {
      filter(hospitals.t1, state== input$state.t1)}
    
    else if (input$beds.t1 == "Any" & input$state.t1 == "All" & input$sector.t1 != "All") {
      filter(hospitals.t1, sector== input$sector.t1)}
    
    else {
      hospitals.t1= hospitals.t1} 
    
    
  })
  
  # hospital icons to a reactive function
  
  HospitalIcons_r <- reactive({
    if( input$sector.t1 == "All") {
      HospitalIcons
    }
    else {
      HospitalIcons[input$sector.t1]
    }
    
  })
  
  # content for the popup
  
  content <- ~paste("<b><a href='", website ,"'>", hospital_name, "</a></b>", "<br>" ,street_address,suburb,state,
                    postcode, "<br>", "<b>Phone:</b>",phone_number,"<br>",
                    "Description:",description,"<br>", sep = " ")
  
  
  # generating a map
  
  output$map <- renderLeaflet({
    
    leaflet(hospitals.t1) %>%
      addTiles()%>%
      addAwesomeMarkers(icon = ~HospitalIcons)%>%
      addLegendAwesomeIcon(iconSet = HospitalIcons,
                           orientation = 'vertical',
                           title = tags$div(
                             style = 'font-size: 10px;',
                             'Sector'),
                           labelStyle = 'font-size: 10px;',
                           position = 'topright')
    
  })
  
  observe({leafletProxy("map" , data = hospital_r()) %>%
      clearMarkers()-> proxy
    if (nrow(hospital_r()) != 0){
      proxy %>%
        addAwesomeMarkers(icon = ~ HospitalIcons_r(), popup = content, label = ~htmlEscape(hospital_name))}
  })
  
  # text output if no selection
  
  output$message <- renderUI(if(nrow(hospital_r()) == 0){
    HTML(paste("<strong>NO HOSPITALS FOUND FOR THIS SELECTION</strong>"))
  })
  
  #Task 2
  # create eventReactive inputs
  state <- eventReactive(input$Task2.update, {input$Task2.state})
  maxdistance <- eventReactive(input$Task2.update, {input$Task2.max.distance})
  npoints <- eventReactive(input$Task2.update, {input$Task2.npoints})
  markers <- eventReactive(input$Task2.update, {input$Task2.show.markers})
  
  # render map
  output$ed.plot <- renderLeaflet({
    # define state polygon
    polys_2 <- subset(state_polysdf, state_polysdf@data$STE_NAME16 == state())
    # create a grid
    grid_2 <- makegrid(polys_2, n = npoints())
    # take the grid and return the distance between the first two points
    gridresolution <- sp::spDists(data.matrix(grid_2[1,]), data.matrix(grid_2[2,]))
    # create SPDF from grid_2
    grid.points <- sp::SpatialPointsDataFrame(grid_2, 
                                              data.frame(n = 1:nrow(grid_2)), 
                                              #CRS(proj4string(polys_2)), doesn't work, replaced:
                                              proj4string = polys_2@proj4string) 
    grid.points <- grid.points[polys_2, ]
    # create dataset of each emergency hospital by state
    state_hospitals_ed <- data_ed[data_ed$state == state(), ]
    # assign coordinate columns
    coordinates(state_hospitals_ed) <- c("longitude", "latitude")
    # create SPDF from hospitals dataset
    state_hospitalSPDF <- sp::SpatialPointsDataFrame(
      coords = coordinates(state_hospitals_ed),
      data.frame(n = 1:nrow(state_hospitals_ed)),
      proj4string = polys_2@proj4string)
    # create matrix of distances
    distance.matrix <- sp::spDists(grid.points, state_hospitalSPDF)
    # find min distance to ED
    grid.points$ed.distance = apply(distance.matrix, 1, min)
    # create grid squares
    grid.squares <- rgeos::gBuffer(grid.points, 
                                   width = gridresolution / 2, 
                                   quadsegs = 1, 
                                   capStyle = "SQUARE", 
                                   byid = TRUE)
    # create colour palette
    palette_1 <- colorNumeric(palette = c("red", "orange", "yellow", "white"), 
                              domain = c(0, maxdistance()),
                              na.color = "white")
    # create hospital icons
    hospital_icons_2 <- makeAwesomeIcon(icon = 'ambulance', markerColor = 'red', iconColor = 'black', library = "fa")
    
    # create map
    ed.plot <- leaflet() %>%
      # use plain background to preserve heatmap colours
      addProviderTiles(providers$CartoDB.Positron) %>%
      # add states, outlined in blue
      addPolygons(data = polys_2,
                  weight = 2,
                  fill = FALSE,
                  color = "blue") %>%
      # add heatmap grid squares
      addPolygons(data = grid.squares, 
                  fillColor = palette_1(grid.points$ed.distance),
                  fillOpacity = 0.5,
                  stroke = FALSE) %>%
      # add legend

      addLegend(position = "bottomleft", 
                pal = palette_1,
                values = seq(0, maxdistance()),
                labFormat = labelFormat(suffix='km'),
                title = "Distance",
                opacity = 2)
    # add markers option
    if (markers()) {
      ed.plot <- ed.plot %>% 
        addAwesomeMarkers(data=data_ed[data_ed$state == state(),],
                          icon = hospital_icons_2,
                          lat = ~latitude,
                          lng = ~longitude,
                          popup = ~paste("<br>Hospital:", i_hospital_name,
                                         "<br>Address:", street_address, suburb,",", state,",", postcode,
                                         "<br>Phone number:", phone_number))

    }
    ed.plot
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

