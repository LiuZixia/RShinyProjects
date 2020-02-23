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
            sliderInput("coast",
                        "Definition of coastal region (distance to coastline km)",
                        min = 10,
                        max = 200,
                        value = 50),
            sliderInput("inland",
                        "Definition of inland region (distance to coastline km)",
                        min = 10,
                        max = 300,
                        value = 100),
            sliderInput("youngest",
                        "Select the age group lower limit (age=(group-1)*5)",
                        min = 1,
                        max = 19,
                        value = 8),
            sliderInput("oldest",
                        "Select the age group higher limit (age=group*5)",
                        min = 1,
                        max = 19,
                        value = 16),
            selectInput(
                "dataset",
                "Set the region for mapping",
                c("Global", "Asia", "Europe", "NorthAmerica"),
                selected = "NorthAmerica",
                multiple = FALSE
            ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("BoxPlot"),
            textOutput("text_output"),
        )
    )
))
