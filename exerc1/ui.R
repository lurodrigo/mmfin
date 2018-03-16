
fluidPage(theme = shinytheme("flatly"),   
  
  # Give the page a title
  titlePanel("Exercício 1"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      helpText("Parâmetros"),
      numericInput("n", "N = ", 10, min = 1),
      numericInput("u", "U = ", 2, step = .01, min = .01),
      textOutput("d"),
      hr(),
      actionButton("refresh", "Gerar de novo")
    ),
    mainPanel(
      highchartOutput("plot")  
    )
    
  )
)
