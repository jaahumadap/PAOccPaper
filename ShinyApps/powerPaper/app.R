# R code for power app

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(png)
library(grid)


ui <- fluidPage(theme = "style.css",
                tags$head(
                        tags$style(HTML("body{ overflow-x: hidden !important; padding: 0px 10px !important;}"))),
                
                titlePanel(tags$strong(tags$h1("How much change can you detect in your occupancy-based monitoring project?"))),
                
                #tags$hr(),
        
                fluidRow(wellPanel(tags$p("Success of a monitoring project based on species occupancy  depends largely on the ability of the monitoring system to detect a given level of change. With this app you can interactively explore how various design features of a monitoring project affect the ability to detect a given level of change in occupancy under various conditions: number of points sampled per year, number of days the sensors are running, the initial occupancy of the species, and the detection probability. The horizontal line in the graph shows the number of years it takes to detect the selected amount of change in occupancy."))),
        
                fluidRow(
                        #column(12, htmlOutput("result"), offset = 4),
                        column(5,
                                selectInput(inputId = "change", label = "How much annual change you want to detect?", choices = c("1%","5%","10%","15%"),selected = "5%"),
                                sliderInput(inputId = "points", label = "How many sampling points are you deploying?", value = 10, min = 0, max = 120, step = 10),
                                sliderInput(inputId = "days", label = "How many days are you leaving your sensors in the field?", value = 30, min = 15, max = 60, step = 15),
                                sliderInput(inputId = "psi1", label = "What is the initial occupancy of the species?", value = 0.5, min = 0.1, max = 0.9, step = 0.2),
                                selectInput(inputId = "p", label = "What is the detection probability of the species?", choices = c(0.1, 0.2, 0.5), selected = 0.5),
                       
                                tags$hr()),
                
                        column(7,plotOutput("plot")))
)


powerData <- read.csv("det.year_2016-08-06_FINAL.csv", h=T)

#data generator function
#Simulate trend function


server <- function(input, output, session){
        #Get the year
        myYear <- reactive({
                row <- filter(powerData, pts == input$points, days == input$days, psi1 == input$psi1, p == input$p, percent == input$change)
                mydata <- row$year1st
                if(is.na(mydata)){
                        mytext <- "more than 10"
                        myvalue <- 11
                }
                else {
                        mytext <- mydata
                        myvalue <- mydata
                }
                list(mytext = mytext, myvalue = myvalue)
        })
        
        
        

        
        
        output$plot <- renderPlot({
                ggplot(filter(powerData, percent == input$change), aes(x=pts, y = year1st, color = psi1, shape = as.character(p))) + geom_jitter(size=3)  + geom_hline(yintercept = myYear()$myvalue, linetype = 2, size = 1.5) +  xlab("Number of camera trap points") + ylab("Number of years to detect change") + scale_shape_discrete(guide = guide_legend(title = "Det. probability")) + scale_color_gradient(guide = guide_legend(title = "Initial\noccupancy")) + ggtitle(paste(myYear()$mytext," years of data to detect\n an annual change of ",input$change))+theme(plot.title = element_text(size=rel(2), face="bold"), axis.title.y = element_text(size=rel(2)), axis.title.x = element_text(size=rel(2)),axis.text = element_text(size=rel(1.5)), legend.text = element_text(size=rel(1.2)), legend.title = element_text(size=rel(1.3)) )
        })
        
        
}

shinyApp(ui = ui, server = server)
