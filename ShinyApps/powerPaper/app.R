# R code for power app
# Jorge A. Ahumada -- Conservation International 

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(png)
library(grid)
library(tidyverse)


ui <- fluidPage(theme = "style.css",
                tags$head(tags$style(HTML("body{ overflow-x: hidden !important; padding: 0px 10px !important;}"))),
                
        fluidRow(
                
                column(10, align = "left",
                       titlePanel(tags$strong(tags$h1("PowerSensor!"),tags$h2("How much change can you detect in your occupancy-based monitoring project?")))
                        ),
                column(2,
                        tags$img(height = 28*3, width = 42*3, src = "SmallLogo.png")
                        )
                ),
        
                fluidRow(
                        wellPanel(tags$p("Success of Occupancy-based species monitoring programs depends largely on the ability to detect a given level of change. Sensitivity to change is a sampling issue but also depends on initial species occupancy and detection probability; the more intense the sampling and the more common and detectable the species is the more sensitive the survey is to detect change. With the PowerSensor! app, you can interactively explore how the various design features of a monitoring program and species properties affect the ability to detect a given level of change in occupancy. This app allows you manipulate the number of points sampled per season, number of days the sensors are running, and initial characteristics of the species, including initial occupancy and detection probability. The output is a graph of the null dynamic occupancy model over seasons (denoted as years) with confidence intervals, and a vertical line highlighting the estimated number of years to detect the trend under the conditions of the simulation."))
                ),
        
        fluidRow(
                column(4, align = "center", 
                        tags$br(),
                        tags$h4("Expected change and power", style = "color:blue"),
                        tags$hr(),
                       wellPanel(
                        selectInput(inputId = "change", label = "How much annual change you want to detect?", choices = c("1%","5%","10%","15%"),selected = "5%"),
                        selectInput(inputId = "alpha", label = "What confidence interval do you want to use to detect change?", choices = c("80%","90%","95%"),selected = "80%")
                       )
                       ),
                
                column(4,align = "center",
                       tags$br(),
                       tags$h4("Field design parameters", style = "color:blue"),
                       tags$hr(),
                       wellPanel(
                               selectInput(inputId = "points",label = "How many sampling points are you deploying?", choices = c(1,2,3,4,5,10,20,30,40,50,60,70,80,90,100,110,120), selected = 30),
                               selectInput(inputId = "days", label = "How many days are you leaving your sensors in the field?", choices = c(1,2,10,20,30,40,50,60), selected = 10)
                       )
                       ),
                
                column(4, align = "center",
                       tags$br(),
                       tags$h4("Species parameters", style = "color:blue"),
                       tags$hr(),
                       wellPanel(
                        sliderInput(inputId = "psi1", label = "What is the initial occupancy of the species?", value = 0.5, min = 0.1, max = 1, step = 0.1),
                        sliderInput(inputId = "p", label = "What is the detection probability of the species?", value = 0.5, min = 0.1, max = 1, step = 0.1)
                       )
                       )
        
                ),
        
        fluidRow(
                column(12,align = "center", tags$hr(), h4(tags$strong(textOutput("message"))), plotOutput("plot"), tags$hr()))
        )


powerData <- readRDS("AllSimulations.rds")
powerData$percent <- NA
powerData$percent[which(powerData$phi == 0.99)] <- "1%"
powerData$percent[which(powerData$phi == 0.95)] <- "5%"
powerData$percent[which(powerData$phi == 0.90)] <- "10%"
powerData$percent[which(powerData$phi == 0.85)] <- "15%"

#data generator function
#Simulate trend function


server <- function(input, output, session){
        #Get the data
        myData <- reactive({
                row <- filter(powerData, pts == input$points, days == input$days, psi1 == input$psi1, p == input$p, percent == input$change)
                
                myPts <- row$pts
                
                if(input$alpha == "80%") {
                        myyear <- row$z.first80
                        col <- "z.first80"
                        lowerCI <- row[9:18]
                        higherCI <- row[19:28]
                        mean <- row[29:38]
                        median <- row[39:48]
                }
                else if(input$alpha == "90%") {
                        myyear <- row$z.first90
                        col <- "z.first90"
                        lowerCI <- row[49:58]
                        higherCI <- row[59:68]
                        mean <- row[69:78]
                        median <- row[79:88]
                }
                
                else {
                        myyear <- row$z.first95
                        col <- "z.first95"
                        lowerCI <- row[89:98]
                        higherCI <- row[99:108]
                        mean <- row[109:118]
                        median <- row[119:128]
                }
                
                
                if(!myyear){
                        mytext <- "more than 10"
                        myvalue <- 11
                }
                else {
                        mytext <- myyear + 1
                        myvalue <- myyear + 1
                }
                
                list(mytext = mytext, myvalue = myvalue, col =col, myPts = myPts, lowerCI = lowerCI, higherCI = higherCI,
                     mean = mean, median = median)
        })
        
        
        

        
        
        output$plot <- renderPlot({
               
                graphData <- data.frame(year = 1:10, mean = t(myData()$mean), median = t(myData()$median), lo = t(myData()$lowerCI), hi = t(myData()$higherCI))
        ggplot(graphData, aes(x=year)) + geom_line(aes(y=mean),size=2) + geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.2) + xlab("Time period") + ylab("Occupancy") + ylim(0,1) +  geom_vline(xintercept = myData()$myvalue) + theme(plot.title = element_text(size=rel(1.5), face="bold"), axis.text = element_text(size = rel(1.5), face = "bold"), axis.title = element_text(size=rel(1.5), face="bold")) + scale_x_continuous(breaks=seq(1, 10, 1))
        })
        
        
        output$message <- renderText({paste(myData()$mytext," years of data to detect an annual change of ",input$change)})

}

shinyApp(ui = ui, server = server)
