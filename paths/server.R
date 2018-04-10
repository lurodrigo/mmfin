
# Define a server for the Shiny app
function(input, output, session) {

  output$diag = renderGrViz({
    graphSpec(input$N, input$u, input$r, input$strike, input$digitos) %>%
      grViz()
  })
  
  output$plot = renderHighchart({
    v = computeVs(input$N, input$u, input$r, input$strike)
    base = rep(NA, input$N+1)
      
    plt = highchart() %>%
      hc_xAxis(categories = 0:(input$N)) %>%
      hc_yAxis(title = list(text = "Preço da opção"), type = "logarithmic") %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_legend(enabled = FALSE)
    
    for (n in (input$N-1):0) {
      segment = base
      for (k in 0:n) {
        segment[n+1] = v[n+1, k+1]
        segment[n+2] = v[n+2, k+1]
        plt %<>% hc_add_series(data = segment, colorIndex = 0, marker = list(symbol = "circle"))
        segment[n+2] = v[n+2, k+2]
        plt %<>% hc_add_series(data = segment, colorIndex = 0, marker = list(symbol = "circle"))
      }
    }
    
    plt
  })
}