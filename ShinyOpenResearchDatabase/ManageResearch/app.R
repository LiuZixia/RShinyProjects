#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#MySQL Connection
source('../sql_conf.R')

#Get contact list from database
Query_Get_Contacts <- "SELECT id, first_name, last_name FROM contact WHERE 1;"
Contact_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Contacts), n=-1)
Contact_List <- split(Contact_List_Raw$id, paste(Contact_List_Raw$first_name, Contact_List_Raw$last_name))

#Get research list from database
Query_Get_Research <- "SELECT * FROM research WHERE 1;"
Research_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Research))

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
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput('Research_List')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  output$Research_List <- renderDataTable(Research_List_Raw)
}

# Run the application 
shinyApp(ui = ui, server = server)

