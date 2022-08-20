library(shiny)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(janitor)
library(tidyverse)

# Set-up fixed data
hosp<-read.csv("data/hospitals.csv")

#Check if any beds are empty
index_empty_bed <- which(hosp$Beds == '')

# All with missing beds are day surgery centres. Other/missing is therefore more appropriate than 'missing'
hosp$Beds[index_empty_bed] <- "Other/Missing" 

# tidy up the data
hosp1 <- hosp %>%
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
         sector=factor(sector),
         beds=factor(beds, ordered = is.ordered(beds), levels = c('<50', '50-99', '100-199', '200-500', '>500', 'Other/Missing'))) %>% 
  rename(hospital = hospital_name) %>%
  #remove unnecessary columns 
  select(-c(local_hospital_network_lhn, primary_health_network_area_phn))

#Create different coloured hospital icons
HospitalIcons <- awesomeIconList(
  Public = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'green', iconColor = 'white', library = "fa"),
  Private = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'orange', iconColor = 'white', library = "fa")
)


# TASK 2 specific 

# define hospitals with emergency departments
table(hosp1$description) # all have "emergency" in description field
# create flag ('ed') for hospitals with emergency dept
hosp2 <- hosp1 %>%
  mutate(ed = str_detect(description, "emergency"))
# subset to hospitals with ED only
ed_only <- filter(hosp2, ed == TRUE)

# read in the polygons
#unzip files 
unzip(zipfile = 'data/1270055001_ste_2016_aust_shape.zip')
polys <- rgdal::readOGR("STE_2016_AUST.shp")
# simplify the polygons, but warning: this step changes SP dataframe to SP file
state_polys <- rgeos::gSimplify(polys, tol = 0.01)
# add data back to polygons to make SP dataframe
state_polysdf <- SpatialPolygonsDataFrame(state_polys, polys@data)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    tabPanel("Task 1 — Hospitals"),
    tabPanel("Task 2 — Distance from Emergency Department", p(),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Task2.state",
                   label = "State",
                   choices = levels(ed_only$state)
                 ),
                 numericInput(
                   inputId = "Task2.max.distance",
                   label = "Maximum distance (in km, 0-1000)",
                   value = 250,
                   min = 0,
                   max = 1000,
                   step = 25
                 ),
                 numericInput(
                   inputId = "Task2.npoints",
                   label = "Number of grid points (1000-10,000)",
                   value = 10000,
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
                 leafletOutput("map2")
                 )
             )
             
    ),
    tabPanel("Task 3 — Length of Hospital Stay")
  )
)

# Define server logic
server <- function(input, output) {
# TASK 2
  
  # create eventReactive inputs
  state <- eventReactive(input$Task2.update, {input$Task2.state})
  maxd <- eventReactive(input$Task2.update, {input$Task2.max.distance})
  npoints <- eventReactive(input$Task2.update, {input$Task2.npoints})
  markers <- eventReactive(input$Task2.update, {input$Task2.show.markers})
  # render map
  output$map2 <- renderLeaflet({
    # define state polygon
    poly2 <- subset(state_polysdf, state_polysdf@data$STE_NAME16 == state())
    # create a grid
    grid2 <- makegrid(poly2, n = npoints())
    # take the grid and return the distance between the first two points
    gridresolution <- sp::spDists(data.matrix(grid2[1,]), data.matrix(grid2[2,]))
    # create SPDF from grid2
    grid.points <- sp::SpatialPointsDataFrame(grid2, 
                                              data.frame(n = 1:nrow(grid2)), 
                                              #CRS(proj4string(poly2)), doesn't work, replaced:
                                              proj4string = poly2@proj4string) 
    grid.points <- grid.points[poly2, ]
    # create dataset of each emergency hospital by state
    state_hospitals <- ed_only[ed_only$state == state(), ]
    # assign coordinate columns
    coordinates(state_hospitals) <- c("longitude", "latitude")
    # create SPDF from hospitals dataset
    state_hospitalSPDF <- sp::SpatialPointsDataFrame(
      coords = coordinates(state_hospitals),
      data.frame(n = 1:nrow(state_hospitals)),
      proj4string = poly2@proj4string)
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
    pal2 <- colorNumeric(palette = c("red", "orange", "yellow", "white"), 
                         domain = c(0, maxd()),
                         na.color = "white")
    # create hospital icons
    HospitalIcons2 <- makeAwesomeIcon(icon = 'hospital-o', markerColor = 'red', iconColor = 'white', library = "fa")
  
    # create map
    m <- leaflet() %>%
      # use plain background to preserve heatmap colours
      addProviderTiles(providers$CartoDB.Positron) %>%
      # add states, outlined in blue
      addPolygons(data = poly2,
                  weight = 2,
                  fill = FALSE,
                  color = "blue") %>%
      # add heatmap grid squares
      addPolygons(data = grid.squares, 
                  fillColor = pal2(grid.points$ed.distance),
                  fillOpacity = 0.5,
                  stroke = FALSE) %>%
      # add legend
      addLegend(position = "bottomleft", 
              pal = pal2,
              values = seq(0, maxd()),
              labFormat = labelFormat(suffix='km'),
              title = "Distance",
              opacity = 2)
      # add markers option
      if (markers()) {
        m <- m %>% 
          addAwesomeMarkers(data=ed_only[ed_only$state == state(),],
                   icon = HospitalIcons2,
                   lat = ~latitude,
                   lng = ~longitude,
                   label = ~hospital)
      }
      m
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
