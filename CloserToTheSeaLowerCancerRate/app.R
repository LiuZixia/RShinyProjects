#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Epidemiological Evidence of SSA Potential Effects on Lung Cancer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
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
      sliderInput("range",
                  "Definition of influence range of SSA (distance to coastline km)",
                  min = 10,
                  max = 300,
                  value = 150),
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
      checkboxGroupInput("region",
                         "Region for analysis (Unselect Global if others are selected)",
                         c("Global", "Europe", "Asia", "North America"), selected=c("Global"),
                         inline = T)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distBoxPlot"),
      textOutput("text_output"),
      plotOutput("distRegPlot"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(data.table)
  library(BSDA)
  library(ggplot2)
  library(ggpmisc)
  
  region_list <- read.csv(file = "./data/region_info_with_distance.csv", 
                          header =T)
  data_raw <- read.csv(file = "./data/CI5-XId/processed/all_sex_group.csv", 
                       header =T)
  data <- reactive({
    data_with_age_group <- data_raw[which(data_raw$age_group > input$youngest & 
                                            data_raw$age_group < input$oldest),]
    data_with_age_group <- aggregate(data_with_age_group[c("number_of_cases", "person_years_at_risk")], 
                                     by = list(registry_code = data_with_age_group$registry_code), FUN = sum)
    data_with_age_group["incidence_rate_100000"] <- 
      data_with_age_group$number_of_cases/data_with_age_group$person_years_at_risk*100000
    data_with_distance <- merge(data_with_age_group, region_list, 
                                by = "registry_code")[,c("incidence_rate_100000", 
                                                         "discription", "distance")]
    data_with_region_group <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(data_with_region_group) <- c("incidence_rate_100000",
                                          "registry_code", "distance",
                                          "type", "region", "p_value")
    for(i in input$region){
      if(i == "Global"){
        DOI <- data_with_distance
      }else if(i == "Europe"){
        DOI <- data_with_distance[data_with_distance$registry_code %in% grep("^5",data_with_coordinate$registry_code,value=T),]
      }else if(i == "Asia"){
        DOI <- data_with_distance[data_with_distance$registry_code %in% grep("^4",data_with_coordinate$registry_code,value=T),]
      }else if(i == "North America"){
        DOI <- data_with_distance[data_with_distance$registry_code %in% grep("^3",data_with_coordinate$registry_code,value=T),]
      }
      
      DOI_coast <- DOI[which(DOI$distance < input$coast*1000),]
      DOI_coast["type"] <- rep("coast", length(DOI_coast[,1]))
      DOI_inland <- DOI[which(DOI$distance >= input$inland*1000),]
      DOI_inland["type"] <- rep("inland", length(DOI_inland[,1]))
      DOI_compare <- rbind(DOI_coast,DOI_inland)
      DOI_compare["region"] <- rep(i, length(DOI_compare[,1]))
      test_results <- t.test(DOI_inland$incidence_rate_100000, 
                             DOI_coast$incidence_rate_100000, var.equal = F)
      DOI_compare["p_value"] <- rep(round(test_results$p.value, digits = 2), length(DOI_compare[,1]))
      data_with_region_group <- rbind(data_with_region_group, DOI_compare)
    }
    data <- data_with_region_group
  })
  data_lm <- reactive({
    data_with_region_group <- data()
    data_lm <- data_with_region_group[which(data_with_region_group$distance<input$range*1000),]
  })
  text_output <- reactive({
    temp <- data()
    text_output_temp <- "P-value of each group:"
    for(i in input$region){
      text_output_temp <- paste(text_output_temp,i,temp[which(temp$region==i),]$p_value[1],";")
    }
    text_output <- text_output_temp
  })
  output$distBoxPlot <- renderPlot({
    ggplot(data(), aes(x = region, color = type,
                       y = incidence_rate_100000),)+
      geom_boxplot()
  })
  output$text_output <- renderText({text_output()})
  output$distRegPlot <- renderPlot({
    ggplot(data_lm(), aes(x = distance/1000, y = incidence_rate_100000))+
      geom_smooth(method='lm', formula= y~x)+
      facet_grid(cols = vars(region))+
      labs(y= "Incidence Rate (100,000)", x = "Distance (km)")+
      stat_poly_eq(formula = y~x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE, size = 3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)