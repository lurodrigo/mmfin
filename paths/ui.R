
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
      h5("Parâmetros"),
      numericInput("N", "N = ", 4, min = 1),
      numericInput("u", "u = ", 1.2, step = .0001, min = .0001)
    ), 
    column(5, offset = 1,
      numericInput("r", "r = ", 1.1, step = .0001, min = .0001),
      numericInput("strike", "strike = ", 1.2, step = .0001, min = .0001)
    )
  ),
  hr(),
  grVizOutput("plot", width = 700, height = 300)
)
