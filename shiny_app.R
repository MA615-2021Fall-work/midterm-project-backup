library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(shinyjs)
library(shinycssloaders)
source("ultimate data wrangle and clean.R")
source("eda.R")




ui <- fluidPage(
  # the navbarpage
    navbarPage("The relationship between stawberry and pesticides",theme = shinytheme("lumen"),
             # Add row choices
              tabPanel("Stawberry Summary", fluid = TRUE,icon = icon("chart-bar"),
                titlePanel("EDA for Strawberry"),
                sidebarLayout(
                  sidebarPanel(
        # Select state
                    titlePanel("Desired Year and Measurement"),
                    fluidRow(
                          #radioButtons(inputId ="StateFinder",
                               #        label = "Choose one state",
                               #        choices= list("CALIFORNIA",
                               #                      "FLORIDA",
                                 #                    "OREGON",
                                 #                    "WASHINGTON"
                                  #                    )
                                  #      ),
            # Select year
                            checkboxGroupInput(inputId ="YearFinder",
                                               label = "MUST choose at least  one year",
                                               choices  = c("2019",
                                                            "2020",
                                                            "2021"
                                                            )
                                                ),
            # Select measurement
                            checkboxGroupInput(inputId ="MeasureFinder",
                                               label = "MUST choose at least one Measurement",
                                               choices  = c(" MEASURED IN LB / ACRE / YEAR  AVG",
                                                            " MEASURED IN PCT OF AREA BEARING  AVG",
                                                            " MEASURED IN LB ",
                                                            " MEASURED IN LB / ACRE / APPLICATION  AVG",
                                                            " MEASURED IN NUMBER  AVG"
                                                            )
                                                ),
            helpText("Tip: If the plot is empty, this means that there is no avaiable data under this filter conditions.")
            
                          #actionButton("draw", "Filter!")
                                  )
                              ),
    # The major interactive plot
                  mainPanel(
                            plotOutput(outputId = "scatterplotFinder",click = "plot_click"),
                            verbatimTextOutput("info"),
                            helpText("Tip: Clicking the plot will show specific x,y value.")
                            )
                          )
              ),
              tabPanel("Location", fluid = TRUE,icon = icon("globe-americas"),
                       titlePanel("Geographical Location Information"),
                       sidebarLayout(
                         sidebarPanel(
                           titlePanel("Desired State"),
                           fluidRow(
                                    checkboxGroupInput(inputId ="StateFinder",
                                                       label = "MUST choose at least one state",
                                                       choices= list("CALIFORNIA",
                                                                     "FLORIDA",
                                                                     "OREGON",
                                                                     "WASHINGTON"
                                                                     )
                                                )
                                    )
                                     ),
                         mainPanel(
                           plotOutput(outputId = "GeoplotFinder",click = "plot_click_1"),
                           verbatimTextOutput("info_1"),
                           helpText("Tip: Clicking the plot will show specific longitude and latitude.")
                           
                         )
                       )
                       
              ),
  
  # Add author information
  navbarMenu("More", icon = icon("info-circle"),
             tabPanel("About", fluid = TRUE,
                      column(6,
                             h4(p("About the Project")),
                             h5(p("This project is about the relationship between strawberries and pesticides ")),
                             br()
                      ),
                      column(6,
                             h4(p("About the Author")),
                             h5(p("Group member: Andrew Sisitzky,Chen Xu,Guangze Yu, Yuyang Li."))
                              )
                      )
              )

  )
)

  



server <- function(input, output,session) {
  # Recall the origianl dataset.
  chemical_clean <- data.frame(chemical_clean)
  eda_subset <- data.frame(eda_subset)
  # Render select Input
  
  datasetInput1 <- reactive({
    chemical_clean  %>% 
      filter(measurement %in% input$MeasureFinder,
            Year %in% input$YearFinder
             #State == input$StateFinder)
      )
  })
  datasetInput2 <- reactive({
    eda_subset  %>% 
      filter(State %in% input$StateFinder
      )
  })
  
    output$scatterplotFinder <- renderPlot({
      ggplot(datasetInput1()) +
      geom_point(aes(x = State,y= Value,col = toxicitylevelhuman))+
      scale_color_gradient(low="blue", high="red")+
      xlab("State name") +
      ylab("Value")
    })
    
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    output$GeoplotFinder <- renderPlot({
      ggplot(datasetInput2()) +
        geom_polygon(aes(x=long, y=lat, group=group, fill = mean_toxicity), color="white", size = 0.2) +
        xlab("Longitude") +
        ylab("Laitude")
      
    })
    output$info_1 <- renderText({
      paste0("Longitude=", input$plot_click_1$x, "\nLatitude=", input$plot_click_1$y)
    })
}


shinyApp(ui = ui, server = server)