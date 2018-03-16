
# Define a server for the Shiny app
function(input, output, session) {

  output$plot = renderHighchart({
    input$refresh
    
    s = randomWalk(input$N, input$u)
    xaxis = 0:(input$N) / input$N
    highchart() %>%
      hc_xAxis(categories = xaxis) %>%
      hc_yAxis(title = list(text = "Valor do ativo")) %>%
      hc_add_series(data = s) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_legend(enabled = FALSE)
  })
}