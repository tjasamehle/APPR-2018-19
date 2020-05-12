library(shiny)

shinyUI(fluidPage(
  
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
               mainPanel(dataTableOutput("zadnjastevilka")))
    )
))
