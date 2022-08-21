# Loading libraries
library(shiny)
library(janitor)
library(tidyverse)
library(leaflet)
library(stringr)
library(htmltools)
library(leaflegend)

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



ui <- fluidPage (
  
  titlePanel("HOSPITAL SEARCH"),
  
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
    uiOutput(outputId = "message"))
)

server <- function(input, output, session) {
  
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
}



shinyApp(ui = ui, server = server)

