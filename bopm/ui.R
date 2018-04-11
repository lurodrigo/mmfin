
fluidPage(theme = shinytheme("cosmo"),   
  withMathJax(),
  HTML('<script type="text/x-mathjax-config">
         MathJax.Hub.Config({
           tex2jax: {inlineMath: [["$","$"]]}
         });
       </script>'),
  # Generate a row with a sidebar
  fluidRow(
    column(5, offset = 1,
      h3("Parâmetros"),
      numericInput("N", "N = ", value = 4, min = 1, step = 1),
      numericInput("T", "T (dias) = ", value = 4, min = 1, step = 1),
      numericInput("S0", "S_0 = ", value = 100, step = .0001, min = 0.0001),
      numericInput("u", "u anual = ", value = 2, step = .0001, min = 1.0001)
    ), 
    column(5, offset = 1,
      numericInput("r", "r anual = ", value = 0.01, step = .0001, min = 0, max = 10),
      numericInput("strike", "Strike = ", value = 1.2, step = .0001, min = .0001),
      checkboxInput("convencao", "Convenção linear?", value = FALSE),
      checkboxInput("anual", "Taxas anuais?", value = FALSE)
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Diagrama", 
        grVizOutput("diag", width = 700, height = 700),
        numericInput("digitos", "Dígitos = ", value = 2, step = 1, min = 1)
      ),
      tabPanel("Gráfico", highchartOutput("plot")),
      tabPanel("Random Walk", 
        actionButton("novo", "Gerar Novamente"),
        highchartOutput("walk")
      ),
      tabPanel("Monte Carlo", 
        numericInput("M", "M = ", value = 100, min = 1, step = 1),       
        textOutput("montecarlo")
      )
    )
  )
)
