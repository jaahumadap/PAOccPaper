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
                
                titlePanel(tags$strong(tags$h1("PowerSensor!"),tags$h2("How much change can you detect in your occupancy-based monitoring project?"))),
                
                #tags$hr(),
        
                fluidRow(wellPanel(tags$p("Success of a monitoring project based on species occupancy  depends largely on the ability of the monitoring system to detect a given level of change. With this app you can interactively explore how various design features of a monitoring project affect the ability to detect a given level of change in occupancy under various conditions: number of points sampled per year, number of days the sensors are running, the initial occupancy of the species, and the detection probability. The horizontal line in the graph shows the number of years it takes to detect the selected amount of change in occupancy."))),
        
                fluidRow(
                        #column(12, htmlOutput("result"), offset = 4),
                        column(5,
                                selectInput(inputId = "change", label = "How much annual change you want to detect?", choices = c("1%","5%","10%","15%"),selected = "5%"),
                               selectInput(inputId = "alpha", label = "What confidence interval do you want to use to detect change?", choices = c("80%","90%","95%"),selected = "80%"),
                                sliderInput(inputId = "points", label = "How many sampling points are you deploying?", value = 10, min = 0, max = 120, step = 10),
                                selectInput(inputId = "days", label = "How many days are you leaving your sensors in the field?", choices = c(1,15,30,45,60), selected = 15),
                                sliderInput(inputId = "psi1", label = "What is the initial occupancy of the species?", value = 0.5, min = 0.1, max = 1, step = 0.1),
                                sliderInput(inputId = "p", label = "What is the detection probability of the species?", value = 0.5, min = 0.1, max = 0.5, step = 0.1),
                       
                                tags$hr()),
                
                        column(7,plotOutput("plot")))
)


powerData <- read.csv("det.year_2016-09-04_1CTremoved.csv", h=T)
powerData$percent <- NA
powerData$percent[which(powerData$phi == 0.99)] <- "1%"
powerData$percent[which(powerData$phi == 0.95)] <- "5%"
powerData$percent[which(powerData$phi == 0.90)] <- "10%"
powerData$percent[which(powerData$phi == 0.85)] <- "15%"

#data generator function
#Simulate trend function


server <- function(input, output, session){
        #Get the year
        myYear <- reactive({
                row <- filter(powerData, pts == input$points, days == input$days, psi1 == input$psi1, p == input$p, percent == input$change)
                
                if(input$alpha == "80%") {
                        mydata <- row$z.first80
                        col <- "z.first80"
                }
                else if(input$alpha == "90%") {
                        mydata <- row$z.first90
                        col <- "z.first90"
                }
                else {
                        mydata <- row$z.first95
                        col <- "z.first95"
                }
                
                if(!mydata){
                        mytext <- "more than 10"
                        myvalue <- 11
                }
                else {
                        mytext <- mydata
                        myvalue <- mydata
                }
                
                list(mytext = mytext, myvalue = myvalue, col =col)
        })
        
        
        

        
        
        output$plot <- renderPlot({
                mypowerData <- filter(powerData, percent == input$change)
                ggplot(mypowerData, aes(x=pts, y = get(myYear()$col), color = psi1, shape = as.character(p))) + geom_jitter(size=2)  + geom_hline(yintercept = myYear()$myvalue, linetype = 2, size = 1.5) +  xlab("Number of camera trap points") + ylab("Number of years to detect change") + scale_shape_discrete(guide = guide_legend(title = "Det. probability")) + scale_color_gradient(guide = guide_legend(title = "Initial\noccupancy")) + ggtitle(paste(myYear()$mytext," years of data to detect\n an annual change of ",input$change))+theme(plot.title = element_text(size=rel(2), face="bold"), axis.title.y = element_text(size=rel(2)), axis.title.x = element_text(size=rel(2)),axis.text = element_text(size=rel(1.5)), legend.text = element_text(size=rel(1.2)), legend.title = element_text(size=rel(1.3)) )
        })
        
        
}

shinyApp(ui = ui, server = server)
