
# Define a server for the Shiny app
function(input, output, session) {

  output$plot = renderGrViz({
    d = graphSpec(input$N, input$u, input$r, input$strike)
    grViz(d)
  })
}