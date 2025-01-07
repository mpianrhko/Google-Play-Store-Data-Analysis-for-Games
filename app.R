
# title: "Google Play Store Data Analysis Project"
# author: "Minho Park - Parast"
# Update: "2023"

# build a Shiny app that investigates variables of Google Play Store game category dataset

#
# Set up project
#

library(ggplot2)
library(dplyr)
library(tibble)
library(shiny)
library(rsconnect)

playStore_csv <- read.csv("android-games.csv")

#
# Organize dataset
#

convertKToNumber <- function(kString) {
  
  # replace k to ""
  
  numericString <- sub("k", "", kString, ignore.case = TRUE)
  
  # make it numeric and multiply it by 1000
  # return
  
  return(as.numeric(numericString) * 1000)
}

convertMToNumber <- function(mString) {
  
  # replace m to ""
  
  numericString <- sub("m", "", mString, ignore.case = TRUE)
  
  # make it numeric and multiply by 1000000
  # return
  
  return(as.numeric(numericString) * 1000000)
}

# make new variable for numeric value of 'installs' variable

playStore_csv$numericInstalls <- NA

for (i in 1:nrow(playStore_csv)) { # do it for every row
  
  # check all k and m
  
  stringInstalls <- tolower(as.character(playStore_csv$installs[i]))
  lastChar <- substr(stringInstalls, nchar(stringInstalls), nchar(stringInstalls))
  
  # if last letter is k, use convertKToNumber()
  
  if (lastChar == "k") {
    playStore_csv$numericInstalls[i] <- convertKToNumber(stringInstalls)
    
    # if last letter is m, use convertMToNumber()
    
  } else if (lastChar == "m") {
    playStore_csv$numericInstalls[i] <- convertMToNumber(stringInstalls)
    
    # if neither, put it directly
    
  } else {
    playStore_csv$numericInstalls[i] <- as.numeric(likeString)
  }
}

# make new variable for the rate between high rates and low rates
# (5 + 4) / (1 + 2)

playStore_csv$rateRatio <- NA
playStore_csv$rateRatio <- (playStore_csv$X5.star.ratings + playStore_csv$X4.star.ratings) / (playStore_csv$X2.star.ratings + playStore_csv$X1.star.ratings)

# make tibble with only selected variables

cleanPlayStore <- tibble(select(playStore_csv,
                                
                                rank,
                                title,
                                category,
                                total.ratings,
                                average.rating,
                                rateRatio,
                                numericInstalls,
                                growth..30.days.,
                                growth..60.days.
                                
))

#
# Set up frontend
#

# Define UI

ui <- fluidPage(
  
  # Title
  
  titlePanel("2023 Play Store Games Analysis"),
  tags$p("Author: Minho Park", style = "font-size:14px;"),
  
  tabsetPanel(
    
    # Univariate Analyses Panel
    
    tabPanel("Univariate Analyses",
             sidebarLayout(
               
               # Side Panel
               
               sidebarPanel(
                 selectInput("variable", "Select a Variable:", 
                             choices = c("Total Ratings" = "total.ratings",
                                         "Numeric Installs" = "numericInstalls",
                                         "Average Rating" = "average.rating", 
                                         "Rate Ratio" = "rateRatio")),
                 
                 # Color
                 selectInput("color", "Choose a Color", 
                             choices = c("Black" = "black", "Red" = "red", "Orange" = "orange", "Yellow" = "yellow", "Green" = "green", "Blue" = "blue", "Purple" = "purple", "White" = "white")),
                 
                 # Binwidth
                 sliderInput("binwidth", "Select Bin Width:", min = 1, max = 10000000, value = 1000000),
                 
                 # Checkbox
                 checkboxInput("showMean", "Show Mean", value = FALSE),
                 checkboxInput("showSD", "Show Standard Deviation", value = FALSE),
                 checkboxInput("showFiveNum", "Show Five-Number Summary", value = FALSE),
                 actionButton("update", "Update Graph"),
                 checkboxInput("showExtremes", "Show Highest and Lowest", value = FALSE),
                 checkboxInput("showImage", "Show Image", value = FALSE)
               ),
               
               # Main Panel
               
               mainPanel(
                 
                 plotOutput("histPlot"),
                 verbatimTextOutput("statsOutput"),
                 verbatimTextOutput("extremeValues"),
                 
                 # Check to show image or not
                 
                 conditionalPanel(
                   condition = "input.showImage == true",
                   tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSjgwxnlNnA5JQQV5EZVrhwy23roLplOwB6E0FW6mbo-974Lfyco5q5QFqWLv0_cZH2XUs&usqp=CAU", 
                            style = "width:100%; height:auto;"),
                   tags$p("Transparent Background Video Game Clip Art PNG, Video Games PNG. Clip Art Video Games Portal Game, PNGAAA,",
                          tags$a(href = "https://www.pngaaa.com/detail/1496821", "PNGAAA"))
                 )
               )
             )
    ),
    
    # Multivariate Analyses Panel
    
    tabPanel("Multivariate Analyses",
             sidebarLayout(
               
               # Side Panel
               
               sidebarPanel(
                 
                 # Chooses variablte to use
                 
                 selectInput("multiVar", "Select a Variable for Analysis with Category:", 
                             choices = c("Rate Ratio" = "rateRatio",
                                         "Numeric Installs" = "numericInstalls",
                                         "Growth 30 Days" = "growth..30.days.", 
                                         "Growth 60 Days" = "growth..60.days.")),
                 
                 # Checkbox
                 
                 checkboxInput("showTable", "Show Summary Table", value = FALSE)
               ),
               
               # Main Panel
               
               mainPanel(
                 plotOutput("multiPlot"),
                 
                 # Check to see summary table or not
                 
                 conditionalPanel(
                   condition = "input.showTable == true",
                   dataTableOutput("summaryTable")
                 )
               )
             )
    )
  )
)

#
# Set up backend
#

# Define server logic

server <- function(input, output, session) {
  
# Univariate Analysis
  
  # Make graph
  
  histogramData <- reactive({
    req(input$variable)  # ensure the variable is selected
    ggplot(cleanPlayStore, aes_string(x = input$variable)) +
      geom_histogram(fill = input$color, binwidth = input$binwidth) +
      labs(x = input$variable)
  })
  
  output$histPlot <- renderPlot({
    input$update  # dependency on the action button
    isolate({ histogramData() })
  })
  
  # Find stats data if checkbox is checked
  
  statsData <- reactive({
    req(input$variable)
    varData <- cleanPlayStore[[input$variable]]
    statsList <- list()
    if(input$showMean) statsList$Mean <- mean(varData, na.rm = TRUE)
    if(input$showSD) statsList$SD <- sd(varData, na.rm = TRUE)
    if(input$showFiveNum) {
      fiveNumValues <- fivenum(varData, na.rm = TRUE)
      labeledFiveNum <- setNames(fiveNumValues, c("Minimum", "Lower Hinge", "Median", "Upper Hinge", "Maximum"))
      statsList$FiveNum <- labeledFiveNum
    }
    statsList
  })
  
  output$statsOutput <- renderPrint({
    input$update
    isolate({ statsData() })
  })
  
  # Find the game that follows min and max value of variables
  
  output$extremeValues <- renderPrint({
    if(input$showExtremes) {
      req(input$variable)
      varData <- cleanPlayStore[[input$variable]]
      titleMax <- cleanPlayStore$title[which.max(varData)]
      titleMin <- cleanPlayStore$title[which.min(varData)]
      list("Highest" = titleMax, "Lowest" = titleMin)
    }
  })
  
  
# Multivariate Analysis
  
  # Graph
  
  output$multiPlot <- renderPlot({
    req(input$multiVar)
    ggplot(cleanPlayStore, aes_string(x = "category", y = input$multiVar, fill = "category")) +
      geom_boxplot() +
      labs(title = paste("Category and", input$multiVar), y = "Value") +
      coord_flip()
  })
  
  output$summaryTable <- renderDataTable({
    req(input$multiVar)
    summaryData <- cleanPlayStore %>%
      group_by(category) %>%
      
      # Summary Table dataset
      
      summarise(
        Count = n(),
        Mean = mean(get(input$multiVar), na.rm = TRUE),
        Median = median(get(input$multiVar), na.rm = TRUE),
        SD = sd(get(input$multiVar), na.rm = TRUE),
        Min = min(get(input$multiVar), na.rm = TRUE),
        Max = max(get(input$multiVar), na.rm = TRUE)
      )
    summaryData
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)
