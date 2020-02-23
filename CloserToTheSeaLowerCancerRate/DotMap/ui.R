library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel(
        "Do population near the coast have a lower lung cancer incidence rate?"
    ),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "sex",
                "Set the sex group for mapping",
                c("all", "male", "female"),
                selected = "all",
                multiple = FALSE
            ),
            selectInput(
                "age_group",
                "Set the age group for mapping",
                c(
                    "0-19" = "0-19",
                    "20-44" = "20-44",
                    "45-84" = "45-84",
                    "Over 85" = "85-104"
                ),
                selected = "45-84",
                multiple = FALSE
            ),
            selectInput(
                "dataset",
                "Set the region for mapping",
                c("Global", "Asia", "Europe", "NorthAmerica"),
                selected = "NorthAmerica",
                multiple = FALSE
            ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("DotMap"),)
    )
))
