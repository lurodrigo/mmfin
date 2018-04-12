
N_MAX = 25

escala = function(checked) {
  if (checked) "logarithmic" else "linear"
}

# Define a server for the Shiny app
function(input, output, session) {

  react = reactiveValues()
  
  observeEvent(input$payoff, {
    react$payoff = payoffFunc(input$payoff)
  })
  
  output$diagrama = renderGrViz({
    if (input$N > N_MAX) {
      return()
    }
    
    graphSpec(input$S0, input$N, input$u, input$r, react$payoff, 
              input$digitos, input$convencao, input$anual) %>%
      grViz()
  })
  
  output$plot = renderHighchart({
    if (input$N > N_MAX || (input$u <= 1 + input$r) || (1 + input$r <= (1/input$u))) {
      return(highchart())
    }
    v = computeVs(input$S0, input$N, input$u, input$r, react$payoff, input$convencao, input$anual)
    base = rep(NA, input$N+1)
      
    plt = highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N, title = list(text = "Tempo (dias)")) %>%
      hc_yAxis(title = list(text = "Preço da opção"), type = escala(input$escala_log)) %>%
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
    input$novoRandomWalk
    
    if ((input$u <= 1 + input$r) || (1 + input$r <= (1/input$u))) {
      return(highchart())
    }
    
    S = randomWalk(input$S0, input$N, input$u, input$r, input$convencao, input$anual)
    
    plt = highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N, title = list(text = "Tempo (dias)"))  %>%
      hc_yAxis(title = list(text = "Preço do ativo"), type = escala(input$escala_log)) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_add_series(data = S, name = "Preço do ativo") %>%
      hc_add_series(data = VAtSequence(S, input$u, input$r, react$payoff), name = "Preço da opção")
    
    plt
  })
  
  output$monteCarlo = renderText({
    input$novoMonteCarlo
    
    monte = monteCarlo(input$S0, input$N, input$u, input$r, 
                            react$payoff, input$convencao, input$anual, input$M)
    glue("Resultado por Monte Carlo: {monte}")
  })
}