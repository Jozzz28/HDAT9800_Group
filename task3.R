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
library(ggplot2)
library(tidyverse)
library(ggthemes)


#Task 3 - Read csv file

task3_colnames <- c("hospital",
                    "state",
                    "lhn",
                    "peer_group",
                    "time_period",
                    "category",
                    "total_num_of_stays",
                    "x1",
                    "num_of_overnight_stays",
                    "x2",
                    "perc_of_overnight_stays",
                    "x3",
                    "avg_len_of_stay_days",
                    "x4",
                    "peer_group_avg_days",
                    "x5",
                    "total_overnight_patient_bed_days",
                    "x6")

#Define column format - skipping empty columns
task3_colspec <- cols_only(
  hospital = col_character(),
  state = col_character(),
  lhn = col_character(),
  peer_group = col_character(),
  time_period = col_character(),
  category = col_character(),
  total_num_of_stays = col_character(), #read as character then cast to numeric to avoid skipping rows with thousand separators
  num_of_overnight_stays = col_character(), #read as character then cast to numeric to avoid skipping rows with thousand separators
  perc_of_overnight_stays = col_character(),
  avg_len_of_stay_days = col_double(),
  peer_group_avg_days = col_double(),
  total_overnight_patient_bed_days = col_character()) #read as character then cast to numeric to avoid skipping rows with thousand separators)

#read raw data
len_of_stay_raw <- 
  read_csv("../data/myhospitals-average-length-of-stay-data.csv", 
           col_names = task3_colnames,
           col_types = task3_colspec, skip = 1)  #skip column names
  
#Data cleansing and type casting
len_of_stay_raw <- len_of_stay_raw %>%
           #convert number of stays columns to numeric, values considered n.a are :'<5', '-', 'NP'
           mutate(total_num_of_stays = as.numeric(gsub(",", "", total_num_of_stays))) %>%  
           mutate(num_of_overnight_stays = as.numeric(gsub(",", "", num_of_overnight_stays))) %>%  
           mutate(total_overnight_patient_bed_days = as.numeric(gsub(",", "", total_overnight_patient_bed_days))) %>%  
           #re-level num_of_overnight_stays for plotting
           mutate(num_of_overnight_stays_cat = factor(case_when(is.na(num_of_overnight_stays) ~ "0",
                                                         num_of_overnight_stays == 0 ~ "0",
                                                         num_of_overnight_stays <= 100 ~ "<= 100",
                                                         num_of_overnight_stays <= 200 ~ "101 ~ 200",
                                                         num_of_overnight_stays <= 300 ~ "201 ~ 300",
                                                         num_of_overnight_stays <= 400 ~ "301 ~ 400",
                                                         num_of_overnight_stays > 400 ~ "401 and above"))) %>%
           mutate(num_of_overnight_stays_cat=fct_relevel(num_of_overnight_stays_cat,c("0", "<= 100", "101 ~ 200","201 ~ 300","301 ~ 400","401 and above"))) %>%
           mutate(time_period = factor(time_period))


# Define UI for Task 3
ui <- fluidPage(

    # Application title
    titlePanel("Average length of stay by Peer Group"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
      
      h3("Main Selectors"),
      #Peer Group selector  
      selectInput(
        inputId = "Task3.peer_group",
        label = "Peer Group",
        choices = c("All", unique(len_of_stay_raw$peer_group)),
        selected = 1
      ),
      
      #Category selector
      selectInput(
        inputId = "Task3.category",
        label = "Category",
        choices = c("All", unique(len_of_stay_raw$category)),
        selected = 1
      ),
      
      #Check box to remove y axis limit of maximum 6 days on average length of stay
      hr(),
      h3("Average length of stay limits"),
      p("Data by default shows plotting for hospitals with average length of stay up to 6 days. Tick Show full dataset to view up to 14 days"),
      checkboxInput("Task3.show_full", "Show full dataset"),
      
      #Add relevant clarifications of dataset used
      hr(),
      h3("Notes"),
      p("* Horizontal line represents the peer group average of selected data"),
      p("* Not peered values are ignored")
      
      ),

        # Geom_segment plot for Task 3
      mainPanel(
          br(),
          br(),
          plotOutput("task3plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  task3plot_data <-  reactive({
    
    #Display full dataset if selector are set to default "All"
    data <-len_of_stay_raw
    
    #Filter on peer_group if selected
    if(input$Task3.peer_group != "All"){
      data <- filter(data, peer_group == input$Task3.peer_group)
    }
    
    #Filter on Category if selected
    if(input$Task3.category != "All"){
      data <- filter(data, category == input$Task3.category)
    }
    
    data
    
  })
  
  output$task3plot <- renderPlot({
    
    p <- ggplot(task3plot_data(), aes(time_period, avg_len_of_stay_days)) + 
      
    #add points with radius reflecting number of overnight stays
    geom_point(aes(size = num_of_overnight_stays_cat), shape = 21, color ="light blue", fill = alpha("light blue", .6), stroke = 1) +
    guides(size= guide_legend("Number of Stays")) +
    
    #add peer group average
    geom_segment(task3plot_data(), mapping = 
                   aes(x = as.numeric(time_period)-0.25, xend =as.numeric(time_period)+0.25, y = peer_group_avg_days, yend =peer_group_avg_days,
                       linetype = "Peer group average"), color ="black") +
    scale_linetype_manual("Peer group average",values=c("Peer group average"=1)) +
    #If user selected to see full dataset, change yxis limit to 14, otherwise 6 as default
    ylim(1,if(input$Task3.show_full){14}else{6}) +
    ggthemes::theme_solarized() +
    labs(title = paste0("Average of length of stays of " ,input$Task3.peer_group),
         subtitles = paste0("Category: ", input$Task3.category),
         x="Time period", y="Average length of stay (Days)") 
    p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
