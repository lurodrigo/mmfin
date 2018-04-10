
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
      numericInput("N", "N = ", 4, min = 1),
      numericInput("u", "u = ", 2, step = .0001, min = .0001)
    ), 
    column(5, offset = 1,
      numericInput("r", "r = ", 0.01, step = .0001, min = .0001),
      numericInput("strike", "strike = ", 1.2, step = .0001, min = .0001),
      numericInput("digitos", "digitos = ", 2, step = 1, min = 1)
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Diagrama", grVizOutput("diag", width = 700, height = 700)),
      tabPanel("Gráfico", highchartOutput("plot"))
    )
  )
)
