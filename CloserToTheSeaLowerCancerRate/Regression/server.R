library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(ggpubr)
    
  output$BoxPlot <- renderPlot({
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
        DOI_coast <- DOI[which(DOI$distance < input$coast*1000), ]
        DOI_coast["type"] <- rep("coast", length(DOI_coast[, 1]))
        DOI_inland <- DOI[which(DOI$distance >= input$inland*1000), ]
        DOI_inland["type"] <- rep("inland", length(DOI_inland[, 1]))
        DOI_compare <- rbind(DOI_coast, DOI_inland)
        DOI_compare$age <-
            factor(DOI_compare$age,
                   levels = c('<20', '20-44', '45-84', '>84'))

        ggplot(data = DOI_compare, aes(x = type, y = incidence_rate_100000)) +
            geom_boxplot() +
            facet_wrap(age ~ ., scales = "free", ncol = 4) +
            stat_compare_means(method = "t.test") +
            labs(y = "Incidence Rate (100,000)",
                 x = paste0("Group\nRegion: ", input$dataset, " Gender: ", input$sex))
    })
})
