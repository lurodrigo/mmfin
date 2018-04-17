
N_MAX = 25

escala = function(checked) {
  if (checked) "logarithmic" else "linear"
}

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
    
    graphSpec(input$S0, input$N, react$u, react$r, react$payoff, input$digitos) %>%
      grViz()
  })
  
  output$plot = renderHighchart({
    if (input$N > N_MAX || !react$validParameters ) {
      return(highchart())
    }
 
    base = rep(NA, input$N+1)
    v = computeVs(input$S0, input$N, react$u, react$r, react$payoff)
      
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
      react$S = randomWalk(input$S0, input$N, react$u, react$r)
    }
  })
  
  output$walk = renderHighchart({
    if (!react$validParameters) {
      return(highchart())
    }

    highchart() %>%
      hc_xAxis(categories = (0:(input$T))/input$N, title = list(text = "Tempo (dias)"))  %>%
      hc_yAxis(title = list(text = "Preço do ativo"), type = escala(input$escala_log)) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_add_series(data = react$S, name = "Preço do ativo") %>%
      hc_add_series(data = VAtSequence(react$S, react$u, react$r, react$payoff), name = "Preço da opção")
  })
  
  output$monteCarlo = renderUI({
    if (!react$validParameters) {
      return(NULL)
    }
    
    input$novoMonteCarlo
    
    bopm = VAt(input$S0, react$u, input$N, react$r, react$payoff)
    monte = monteCarlo(input$S0, input$N, react$u, react$r, react$payoff, input$M)
    div(p(sprintf("Valor da opção em t=0 por BOPM: %.2f.", bopm)),
      p(sprintf("\nValor da opção em t=0 por Monte Carlo: %.2f.", monte)))
  })
  
  output$vn = renderHighchart({
    if (!react$validParameters) {
      return(NULL)
    }
    
    N_disc = 30
    
    bopm = map_dbl(1:N_disc, function(n) {
        if (input$anual) {
          u = input$u^(1/360 * input$T/n)
          r = (1 + input$r)^(input$T/360 * input$T/n) - 1
        } else {
          u = input$u^(input$T/n)
          r = (1 + input$r)^(input$T/n) - 1
        }
        
        VAt(input$S0, u, n, r, react$payoff)
      })
    
    monte = map_dbl(1:N_disc, function(n) {
      if (input$anual) {
        u = input$u^(1/360 * input$T/n)
        r = (1 + input$r)^(input$T/360 * input$T/n) - 1
      } else {
        u = input$u^(input$T/n)
        r = (1 + input$r)^(input$T/n) - 1
      }
      
      monteCarlo(input$S0, n, u, r, react$payoff, 1000)
    })
    
    highchart() %>%
      hc_xAxis(categories = 1:N_disc, title = list(text = "N"))  %>%
      hc_yAxis(title = list(text = "Valor da opção")) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_add_series(data = bopm, name = "BOPM") %>%
      hc_add_series(data = monte, name = "Monte Carlo") 
  })
}