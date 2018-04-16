
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
  
  observe({
    react$naParameters = is.na(input$u) || is.na(input$r) || is.na(input$T) || is.na(input$S0) || is.na(input$N)
  })
  
  observe({
    if (input$anual) {
      react$u = input$u^(1/360 * input$T/input$N)
      react$r = (1 + input$r)^(input$T/360 * input$T/input$N) - 1
    } else {
      react$u = input$u^(input$T/input$N)
      react$r = (1 + input$r)^(input$T/input$N) - 1
    }
    
    react$validParameters = !react$naParameters && (react$u > 1 + react$r) && (1 + react$r > (1/react$u))
  })
  
  output$msg = renderUI({
    if (react$naParameters) {
      HTML("<strong>Atenção:</strong> há parâmetros não preenchidos ou inválidos.")
    } else if (!react$validParameters) {
      HTML("<strong>Atenção:</strong> Os valores de u e r devem satisfazer u > 1 + r > 1/u.")
    } else {
      NULL
    }
  })
  
  output$diagrama = renderGrViz({
    if (input$N > N_MAX || !react$validParameters) {
      return()
    }
    
    graphSpec(input$S0, input$N, react$u, react$r, react$payoff, 
              input$digitos, input$anual) %>%
      grViz()
  })
  
  output$plot = renderHighchart({
    if (input$N > N_MAX || !react$validParameters ) {
      return(highchart())
    }
 
    base = rep(NA, input$N+1)
    v = computeVs(input$S0, input$N, react$u, react$r, react$payoff, input$anual)
      
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
  
  observe({
    input$novoRandomWalk
    
    if (react$validParameters) {
      react$S = randomWalk(input$S0, input$N, react$u, react$r, input$anual)
    }
  })
  
  output$walk = renderHighchart({
    if (!react$validParameters) {
      return(highchart())
    }

    plt = highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N, title = list(text = "Tempo (dias)"))  %>%
      hc_yAxis(title = list(text = "Preço do ativo"), type = escala(input$escala_log)) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_add_series(data = react$S, name = "Preço do ativo") %>%
      hc_add_series(data = VAtSequence(react$S, react$u, react$r, react$payoff), name = "Preço da opção")
    
    plt
  })
  
  output$monteCarlo = renderText({
    if (!react$validParameters) {
      return(NULL)
    }
    
    input$novoMonteCarlo
    
    monte = monteCarlo(input$S0, input$N, react$u, react$r, 
                            react$payoff, input$anual, input$M)
    glue("Resultado por Monte Carlo: {monte}")
  })
}