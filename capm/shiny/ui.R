
fluidPage(theme = shinytheme("cosmo"),   
  withMathJax(),
  HTML('<script type="text/x-mathjax-config">
         MathJax.Hub.Config({
           tex2jax: {inlineMath: [["$","$"]]}
         });
       </script>'),
  fluidRow(
    textInput()
  )
)
