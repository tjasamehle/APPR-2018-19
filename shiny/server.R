library(shiny)

shinyServer(function(input, output) {
  output$glavnestevilke <- renderUI(
    selectInput("glavnestevilke", label="Izberi številko",
                choices= sort(unique(tabela2$Stevilka), decreasing = F) )
  )
  output$glavnastevilka <- renderDataTable(if (!is.null(input$glavnestevilke) && input$glavnestevilke %in% sort(unique(tabela2$Stevilka))) {
    tabela_prve %>% filter(Stevilka == input$glavnestevilke)}
    else {tabela_prve}
    )
  
  output$zadnjestevilke <- renderUI(
    selectInput("eurostevilke", label="Izberi številko",
                choices= c(1:10) )
  )
  
  output$zadnjastevilka <- renderDataTable(if (!is.null(input$eurostevilke) && input$eurostevilke %in% c(1:10)) {
    tabela_zadnje %>% filter(Stevilka == input$eurostevilke)}
    else {tabela_zadnje}
  )
  
  # output$naselja <- renderPlot({
  #   main <- "Pogostost števila naselij"
  #   if (!is.null(input$pokrajina) && input$pokrajina %in% levels(obcine$pokrajina)) {
  #     t <- obcine %>% filter(pokrajina == input$pokrajina)
  #     main <- paste(main, "v regiji", input$pokrajina)
  #   } else {
  #     t <- obcine
  #   }
  #   ggplot(t, aes(x=naselja)) + geom_histogram() +
  #     ggtitle(main) + xlab("Število naselij") + ylab("Število občin")
  # })
  
  
  
})   #zapre funkcijo in pred funkcijo
