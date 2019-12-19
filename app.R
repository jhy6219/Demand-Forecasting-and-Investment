library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(flexdashboard)
#rm(list = ls())
source("helpers_hy.R")
###----------------------UI-------------------------###
header <- dashboardHeader(title = "Forecasting movie audience and Investing",
                          titleWidth = 600)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("How to use", tabName = "dashboard", icon = icon("th")),
        menuItem("Let's invest", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
    )
)
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    column(width = 3,
                           box( width = NULL,collapsible=TRUE,
                                title = "Choose movie", status = "warning",
                                selectInput("movie", 
                                            "Korean film produced or distributed at CJ E&M", 
                                            choices = list("Extreme job" = "job",
                                                           "Exit" = "exit",
                                                           "The Divine Move"="god"),
                                            selected = "job")
                           ),
                           box( width = NULL,height=200,
                                title = "Expected movie earning rate", status = "warning",
                                solidHeader = TRUE,
                                gaugeOutput("gauge",height="5000px")
                           ),
                           valueBoxOutput("progressBox")
                    ),
                    column(width=3,height=490,
                           box(width=NULL,height=490,
                               collapsible=TRUE,
                               title = "Movie Poster", status = "warning",
                               imageOutput("myImage"))),
                    column(width = 6,  height=550,
                           box( title = "Setting", solidHeader = TRUE, 
                                collapsible=TRUE,
                                status = "warning",width = NULL,
                                style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                column(width=6, 
                                       selectInput("model", h4("Choose the Diffusion Model"), 
                                                   choices = list("Bass" = "Bass",
                                                                  "Logistic" = "Logistic",
                                                                  "Gumbel" = "Gumbel",
                                                                  "Exponential"= "Exponential")
                                                   , selected = "Bass"),
                                       selectInput("Method",h4("Choose the Estimation Method"),
                                                   choices = list("OLS" = "OLS",
                                                                  "Q-Q Plot" = "Q-Q Plot"),
                                                   selected = "OLS"),
                                       sliderInput("num", "weekend effect", min=1, max=5, value=2), 
                                       #animate=animationOptions(100))
                                       numericInput("day",h4("Days since the release date"),
                                                    value = 10),
                                       numericInput("n",h4("Number of future data"),
                                                    value = 10)
                                ),
                                column(width=6,
                                       dateInput("open",h4("release date"), 
                                                 value = "2019-01-23"),
                                       h4("   Extreme job: 2019.01.23"),
                                       h4("   Exit: 2019.08.02"),
                                       h4("   The Divine Move: 2019.11.15"),
                                       numericInput("bp",h4("break-even point(unit:K)"),
                                                    value = 2300),
                                       h4("   Extreme job: 2300K"),
                                       h4("   Exit: 3500K"),
                                       h4("   The Divine Move: 2300K")
                                       
                                )
                                
                           )
                    )),
                fluidRow(
                    column(width = 4,
                           box( width = NULL,collapsible=TRUE,solidHeader = TRUE,
                                title="Expected audience",status = "danger",
                                plotOutput('plot1')
                           )),
                    column(width = 4,
                           box( width = NULL,collapsible=TRUE,solidHeader = TRUE,
                                title="rate of return in CJ E&M",status = "danger",
                                plotOutput('plot2'))),
                    column(width = 4,
                           box( width = NULL,collapsible=TRUE,solidHeader = TRUE,
                                title="Q-Q Plot",status = "danger",
                                plotOutput('plot3')))
                    
                    
                )),
        
        
        
        
        
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        )
    )
)


ui <- dashboardPage(header,sidebar, body,skin="green")

server <- function(input, output){
    output$myImage <- renderImage({
        if(input$movie=="god"){
            pic1="god.jpg"
        }
        else if(input$movie=="exit"){
            pic1="exit.jpg"
        }
        else{
            pic1="job.jpg"
        }
        return(list(src = pic1,contentType = "image/jpg",alt = "Alignment", 
                    width = 365, height = 440))
    }, deleteFile = FALSE)
    
    
    dfdata <- reactive({
        if(input$movie=="god"){
            week_effect(god,input$num,ymd(input$open),input$day)
        }
        else if(input$movie=="exit"){
            week_effect(exit,input$num,ymd(input$open),input$day)
        }
        else{
            week_effect(job,input$num,ymd(input$open),input$day)
        }
    })
    
    output$gauge=renderGauge({
        myabc=abc(dfdata(),input$model)
        rate <- round(100*mpq(dfdata(),myabc,input$Method,input$model)$m/(1000*input$bp)-100)
        gauge(rate, min = -100, max = 100, symbol = '%', 
              label = "Movie's \n rate of return", 
              gaugeSectors(
                  success = c(50, 100), warning = c(0, 50), danger = c(-100, 0)
              ))
    })
    
    output$progressBox <-  shinydashboard::renderValueBox({
        myabc=abc(dfdata(),input$model)
        m=mpq(dfdata(),myabc,input$Method,input$model)$m
        rate <- round(100*m/(1000*input$bp)-100)
        if (rate<=0){
            text1="Never invest"
            text2=paste0("predicted total audience: ",round(m/1000),"K")
            icon1="thumbs-down"
            color1="red"
        }
        else if (rate<=50){
            text1="Invest in your beliefs."
            text2=paste0("predicted total audience: ",round(m/1000),"K")
            icon1="hand-rock"
            color1="yellow"
        }
        else{
            text1="Invest it quickly"
            text2=paste0("predicted total audience: ",round(m/1000),"K")
            icon1="thumbs-up"
            color1="green"
        }
        shinydashboard::valueBox(
            text1, text2, icon = icon(icon1),
            color = color1
        )
    })
    output$plot1 <- renderPlot({
        if(input$movie=="god"){
            draw_g1(god,input$n,input$num,ymd(input$open),dfdata(),input$model)
        }
        else if(input$movie=="exit"){
            draw_g1(exit,input$n,input$num,ymd(input$open),dfdata(),input$model)
        }
        else{
            draw_g1(job,input$n,input$num,ymd(input$open),dfdata(),input$model)
        }
    })
    output$plot2 <- renderPlot({
        draw_g2(input$open,input$day,input$n)
    })
    output$plot3 <- renderPlot({
        if(input$movie=="god"){
            df100=god
        }
        else if(input$movie=="exit"){
            df100=exit
        }
        else{
            df100=job
        }
        mo=input$model
        data11=week_effect(df100,input$num,input$open,input$day)
        myabc=abc(data11,mo)
        drawqq(mo,data11,mpq(data11,myabc,"OLS",mo))
    })
}

shinyApp(ui, server)

