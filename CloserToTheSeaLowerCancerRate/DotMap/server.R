library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(sf)
    library(tmap)
    
    DOI_map <- reactive({
        data("World")
        if (input$dataset == "Global") {
            DOI_map <-
                as(st_transform(World, "+proj=longlat +datum=WGS84"),
                   "Spatial")
        } else if (input$dataset == "Europe") {
            DOI_map <-
                as(st_transform(World[which(World$continent == "Europe" &
                                                World$name != "Russia"), ], "+proj=longlat +datum=WGS84"),
                   "Spatial")
        } else if (input$dataset == "NorthAmerica") {
            DOI_map <-
                as(st_transform(World[which(World$continent == "North America"), ], "+proj=longlat +datum=WGS84"),
                   "Spatial")
        } else if (input$dataset == "Asia") {
            DOI_map <-
                as(st_transform(World[which(World$continent == "Asia"), ], "+proj=longlat +datum=WGS84"),
                   "Spatial")
        }
    })
    
    incidence_data_sf <- reactive({
        #Read incidence rate data
        incidence_data <- read.csv(
            file = paste0(
                "../data/CI5-XId/processed/subset/",
                input$sex,
                "/",
                input$dataset,
                "_age_group_",
                input$age_group,
                ".csv"
            ),
            header = T
        )[, c("latitude",
              "longitude",
              "incidence_rate_100000",
              "distance")]
        incidence_data$log_distance <-
            log(incidence_data$distance[])
        incidence_data_sf <-
            st_as_sf(
                incidence_data,
                coords = c("longitude", "latitude"),
                crs = "+proj=longlat +datum=WGS84"
            )
    })
    
    output$DotMap <- renderPlot({
        tm_shape(DOI_map()) + tm_polygons() +
            tm_shape(incidence_data_sf()) +
            tm_dots(
                col = "incidence_rate_100000",
                midpoint = TRUE,
                n = 10,
                title = paste0(
                    "Lung Cancer \nIncidence Rate\n",
                    input$dataset,
                    "\nAge ",
                    input$age_group,
                    " ",
                    input$sex
                ),
                size = 0.3
            ) +
            tm_legend(legend.outside = TRUE)
    })
})
