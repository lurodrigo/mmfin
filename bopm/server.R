
# Define a server for the Shiny app
function(input, output, session) {

  output$diag = renderGrViz({
    if (input$N > 10) {
      return()
    }
    
    graphSpec(input$S0, input$N, input$T, input$u, input$r, input$strike, 
              input$digitos, input$convencao, input$anual) %>%
      grViz()
  })
  
  output$plot = renderHighchart({
    if (input$N > 10 || (input$u <= 1 + input$r) || (1 + input$r <= (1/input$u))) {
      return(highchart())
    }
    v = computeVs(input$S0, input$N, input$T, input$u, input$r, input$strike, input$convencao, input$anual)
    base = rep(NA, input$N+1)
      
    plt = highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N) %>%
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
  
  output$walk = renderHighchart({
    if ((input$u <= 1 + input$r) || (1 + input$r <= (1/input$u))) {
      return(highchart())
    }
    
    input$novo

    S = randomWalk(input$S0, input$N, input$T, input$u, input$r, input$strike, input$convencao, input$anual)
    
    plt = highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N) %>%
      hc_yAxis(title = list(text = "Preço do ativo"), type = "logarithmic") %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_series(data = S, colorIndex = 0, marker = list(symbol = "circle"))
    
    plt
  })
  
  output$montecarlo = renderText({
    monte = monteCarlo(input$S0, input$N, input$T, input$u, input$r, 
                            input$strike, input$convencao, input$anual, input$M)
    glue("Resultado por Monte Carlo: {monte}")
  })
}