library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Analiza števil"),
  
  tabsetPanel(
      tabPanel("Analiza glavnih števil",
               sidebarPanel(
                 uiOutput("glavnestevilke")
               ),
               mainPanel(dataTableOutput("glavnastevilka"))),
      
      tabPanel("Analiza euro števil",
               sidebarPanel(
                  uiOutput("zadnjestevilke")
                ),
               mainPanel(dataTableOutput("zadnjastevilka"),
                         dataTableOutput("pojavi")))
    )
))
