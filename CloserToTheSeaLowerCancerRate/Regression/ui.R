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
                "Set the sex group for analysis",
                c("all", "male", "female"),
                selected = "female",
                multiple = FALSE
            ),
            sliderInput("distance",
                        "Definition of coastal region (distance to coastline km)",
                        min = 10,
                        max = 300,
                        value = 150),
            selectInput(
                "dataset",
                "Set the region for analysis",
                c("Global", "Asia", "Europe", "NorthAmerica"),
                selected = "Global",
                multiple = FALSE
            ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Regression"),
        )
    )
))
