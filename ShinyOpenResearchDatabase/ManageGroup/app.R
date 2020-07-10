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

#Get research list from database
Query_Get_Research <- "SELECT id, abbreviation, full_name FROM research WHERE 1;"
Research_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Research), n=-1)
Research_List <- split(Research_List_Raw$id, Research_List_Raw$abbreviation)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Manage Group"),
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      selectInput("research_id", "Research", choices = Research_List, selected = 1),
      textInput("type", "Type:", value = "Enter text..."),
      dateInput("start_date", "Start date:", value = Sys.Date()),
      dateInput("end_date", "End date:", value = Sys.Date()),
      selectInput("info_variables", "Research", choices = Research_List, selected = 1),
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
server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['ResearchID']])) {
      updateTextInput(session, "research_id", value = query[['ResearchID']])
    }
  })
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
  Query_Get_Group <- paste0("SELECT * FROM group WHERE research_id = ", input$research_id, ";")
  Research_List_Raw <- fetch(dbSendQuery(DB_Connection, Query_Get_Research))
  RMySQL::dbDisconnect(DB_Connection)
  Research_List_Raw$id <- paste0('<a href="../ManageGroup/?ResearchID=', Research_List_Raw$id, '">', Research_List_Raw$id, "</a>")
  output$Research_List <- renderDataTable({Research_List_Raw}, escape = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

