
fluidPage(theme = shinytheme("cosmo"),   
  withMathJax(),
  HTML('<script type="text/x-mathjax-config">
         MathJax.Hub.Config({
           tex2jax: {inlineMath: [["$","$"]]}
         });
       </script>'),
  fluidRow(
    h3("Parâmetros"),
    column(4, 
      numericInput("N", "N:", value = 4, min = 1, step = 1),
      numericInput("T", "T (em dias):", value = 4, min = 1, step = 1),
      numericInput("S0", "Preço inicial do ativo (S0):", value = 100, step = .0001, min = 0.0001)
    ), 
    column(4, 
      numericInput("u", "Up (u):", value = 2, step = .0001, min = 1.0001),
      numericInput("r", "Taxa de juros neutra ao risco (r):", value = 0.1, step = .0001, min = 0, max = 10),
      textInput("payoff", "Payoff:", value = "max(S-80, 0)")
    ),
    column(4,
      #checkboxInput("convencao", "Convenção linear?", value = FALSE),
      checkboxInput("anual", "Taxas anuais?", value = FALSE),
      checkboxInput("escala_log", "Eixo Y em log?", value = TRUE),
      uiOutput("msg")
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Random Walk", 
        actionButton("novoRandomWalk", "Gerar Novamente", icon = icon("refresh")),
        highchartOutput("walk")
      ),
      tabPanel("Valor da opção (BOPM/Monte Carlo)", 
        numericInput("M", "M = ", value = 10000, min = 1, step = 1), 
        actionButton("novoMonteCarlo", "Gerar Novamente", icon = icon("refresh")),
        uiOutput("monteCarlo")
      ),
      tabPanel("Diagrama", 
        numericInput("digitos", "Dígitos = ", value = 2, step = 1, min = 1),
        div(style = "text-align: center;", grVizOutput("diagrama", width = 700, height = 700))
      ),
      tabPanel("Gráfico", highchartOutput("plot")),
      tabPanel("V em função de N", 
        actionButton("novoVn", "Gerar Novamente", icon = icon("refresh")),
        highchartOutput("vn"))
    )
  )
)
