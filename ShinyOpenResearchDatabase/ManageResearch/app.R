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

#MySQL Connection
source('../sql_conf.R')

#Get contact list from database
Query_Get_Contacts <- "SELECT id, first_name, last_name FROM contact WHERE 1;"
Contact_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Contacts), n=-1)
Contact_List <- split(Contact_List_Raw$id, paste(Contact_List_Raw$first_name, Contact_List_Raw$last_name))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Manage Research"),
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      textInput("abbreviation", "Abbreviation:", value = "Enter text..."),
      textInput("full_name", "Full name:", value = "Enter text..."),
      textAreaInput("description", "Description:", value = "Enter text..."),
      selectInput("contact_id", ("Contact"), choices = Contact_List, selected = 1),
      dateInput("created_at", "Created at: ", value = Sys.Date()),
      actionButton("submit", "Submit", selected = NULL),
      actionButton("refresh", "Refresh", selected = NULL)
    ),
      
      # Show research list
      mainPanel(
        dataTableOutput('Research_List')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$submit, {
    source('../sql_conf.R')
    Query_Insert_Research <- paste0("INSERT INTO `research` (`id`, `abbreviation`, `full_name`, `created_at`, `modified_at`, `description`, `contact_id`) VALUES (NULL, '",input$abbreviation, "', '",input$full_name, "', current_timestamp(), '0000-00-00 00:00:00.000000', '",input$description, "', '",input$contact_id, "');")
    showNotification(dbSendQuery(DB_Connection, Query_Insert_Research))
    RMySQL::dbDisconnect(DB_Connection)
    shinyjs::js$refresh()
  })
    
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  #Get research list from database
  source('../sql_conf.R')
  Query_Get_Research <- "SELECT * FROM research WHERE 1;"
  Research_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Research))
  RMySQL::dbDisconnect(DB_Connection)
  Research_List_Raw$id <- paste0('<a href="../ManageGroup/?ResearchID=', Research_List_Raw$id, '">', Research_List_Raw$id, "</a>")
  output$Research_List <- renderDataTable({Research_List_Raw}, escape = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

