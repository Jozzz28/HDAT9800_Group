library(shiny)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)



hospitals <- read.csv("data/hospitals.csv")
data_ed <- filter(hospitals,
                  grepl('emergency department', Description))
state_polys = readOGR("data/STE_2016_AUST.shp")
state_polys <- rgeos::gSimplify(state_polys, tol = 0.01)
colour_palette <- colorNumeric(c("red", "orange", "yellow", "white"), c(0, maxdistance))

state_hospitals <- hospitals %>% 
  group_by(State)

grid <- reactive({
  # make a grid spanning the state polygon
  makegrid(state_polys(), n = npoints())
})

gridresolution <- reactive({
  g <- grid()
  sp::spDists(data.matrix(g[1,]), data.matrix(g[2,]))
})

grid.points <- sp::SpatialPointsDataFrame(g, data.frame(n=1:nrow(g)), proj4string = CRS(proj4string(state_polys())))
grid.points <- grid.points[state_polys(), ]

coordinates(data_ed) <- c("Longitude", "Latitude")

gClip <- function(shp, bb){
  if (class(bb) == "matrix") {
    b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  } else {
    b_poly <- as(extent(bb), "SpatialPolygons")
  }
  proj4string(b_poly) = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  raster::crop(shp, bb)
}

proj4string(data_ed) <- CRS(proj4string(state_polys()))

distance.matrix <- sp::spDists(grid.points, state_hospitals)

grid.points$ed.distance = apply(distance.matrix, 1, min)

grid.squares <- rgeos::gBuffer(grid.points, width = gridresolution() / 2, 
                               quadsegs = 1, capStyle = "SQUARE", byid = TRUE)

lng_ed <- state_hospitals$Longitude
lat_ed <- state_hospitals$Latitude

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    
    tabPanel("Task 2 — Distance from Emergency Department", p(),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("Task2.state", "State:", 
                             choices = sort(unique(data_ed$State)),
                             selected = "NSW"),
                 textInput("Task2.maxdistance", "Maximum distance (in km)"),
                 textInput("Task2.npoints", "Number of grid points"),
                 checkboxInput("Task2.show.markers", "Show markers"),
                 actionButton("Task2.update", "Update")
               ),
               
               mainPanel(
                 leafletOutput("ED.plot")
               )
             )
             
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # add your output slot render functions here
  state_subset <- reactive({
    state_hospitals %>%
      filter(State==input$Task2.state)
  })
  
  #Task 2
  output$ED.plot <- renderLeaflet({
    leaflet(data = state_subset)  %>% 
      addAwesomeMarkers(lat = lat_ed, lng = lng_ed, group = "Markers") %>% 
      addPolygons(data = state_polys) %>% 
      addLayersControl(overlayGroups = "Markers",
                       options = layersControlOptions(collapsed = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
