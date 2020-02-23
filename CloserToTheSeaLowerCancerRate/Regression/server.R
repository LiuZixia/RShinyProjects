library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(ggpmisc)
    
  output$Regression <- renderPlot({
        incidence_data_1 <- read.csv(
            file = paste0(
                "../data/CI5-XId/processed/subset/",
                input$sex,
                "/",
                input$dataset,
                "_age_group_0-19.csv"
            ),
            header = T
        )
        incidence_data_1$age <- rep("<20", nrow(incidence_data_1))
        incidence_data_2 <- read.csv(
            file = paste0(
                "../data/CI5-XId/processed/subset/",
                input$sex,
                "/",
                input$dataset,
                "_age_group_20-44.csv"
            ),
            header = T
        )
        incidence_data_2$age <- rep("20-44", nrow(incidence_data_2))
        incidence_data_3 <- read.csv(
            file = paste0(
                "../data/CI5-XId/processed/subset/",
                input$sex,
                "/",
                input$dataset,
                "_age_group_45-84.csv"
            ),
            header = T
        )
        incidence_data_3$age <- rep("45-84", nrow(incidence_data_3))
        incidence_data_4 <- read.csv(
            file = paste0(
                "../data/CI5-XId/processed/subset/",
                input$sex,
                "/",
                input$dataset,
                "_age_group_85-104.csv"
            ),
            header = T
        )
        incidence_data_4$age <- rep(">84", nrow(incidence_data_4))
        DOI <-
            rbind(incidence_data_1,
                  incidence_data_2,
                  incidence_data_3,
                  incidence_data_4)
        DOI$age <-
            factor(DOI$age,
                   levels = c('<20', '20-44', '45-84', '>84'))
        DOI_coast <- DOI[which(DOI$distance < input$distance*1000), ]
        ggplot(data = DOI_coast, aes(x = distance / 1000, y = incidence_rate_100000)) +
          geom_smooth(method = 'lm', formula = y ~ x) +
          facet_wrap(age ~ ., scales = "free", ncol = 4) +
          stat_poly_eq(
            formula = y ~ x,
            aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
            parse = TRUE,
            size = 3
          )+
            labs(y = "Incidence Rate (100,000)",
                 x = paste0("Distance (km)\nRegion: ", input$dataset, " Gender: ", input$sex))
    })
})
