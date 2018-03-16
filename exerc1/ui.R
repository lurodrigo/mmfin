
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
      h4("Exercício 1"), 
      helpText("Aqui, temos que fazer um fazer um random walk definida por $S_0 = 1$ e ",
               "$$S_n = \\begin{cases} 
          uS_{n-1}, & \\text{com probabilidade .5}\\\\
          u^{-1}S_{n-1}, & \\text{com probabilidade .5} \\\\
          \\end{cases}$$"),
      actionButton("refresh", "Gerar de novo") 
    ), 
    column(5, offset = 1,
      h5("Parâmetros"),
      numericInput("N", "N = ", 200, min = 1),
      numericInput("u", "u = ", 1.2, step = .01, min = .01),
      hr()
    )
  ),
  hr(),
  highchartOutput("plot")
)
