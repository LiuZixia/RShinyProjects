#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Manage Contact"),
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      textInput("first_name", "First name:", value = "Enter text..."),
      textInput("middle_name", "Middle name:", value = "Enter text..."),
      textInput("last_name", "Last name:", value = "Enter text..."),
      textInput("position", "Position:", value = "Enter text..."),
      textInput("affiliation", "Affiliation:", value = "Enter text..."),
      actionButton("submit", "Submit", selected = NULL),
      actionButton("refresh", "Refresh", selected = NULL)
    ),
      
      # Show research list
      mainPanel(
        dataTableOutput('Contact_List')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$submit, {
    source('../sql_conf.R')
    Query_Insert_Research <- paste0("INSERT INTO `contact` (`id`, `first_name`, `middle_name`, `last_name`, `position`, `affiliation`) VALUES (NULL, '",input$first_name, "', '",input$middle_name, "','",input$last_name, "', '",input$position, "', '",input$affiliation, "');")
    showNotification(dbSendQuery(DB_Connection, Query_Insert_Research))
    RMySQL::dbDisconnect(DB_Connection)
    shinyjs::js$refresh()
  })
    
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  #Get research list from database
  source('../sql_conf.R')
  Query_Get_Contacts <- "SELECT * FROM contact WHERE 1;"
  Contact_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Contacts))
  output$Contact_List <- renderDataTable({Contact_List_Raw}, escape = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

