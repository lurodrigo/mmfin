library(DiagrammeR)
library(shiny)

server <- function(input, output) {
  output$diagram <- renderDiagrammeR({
    d <- paste(
      paste0("graph ", "TB"),
      paste0("A[Input Data No. of Genes: ", input$ngenes, "] --> B[Filtering Parameters]"),
      "B --> C{A Rhombus}",
      "C --> D[Rectangle One]",
      "C --> E[Rectangle Two]",
      sep = "\n")
    mermaid(d)
  })
}

ui <- fluidPage(
  titlePanel("DiagrammeR + shiny"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Example showing DiagrammeR output in a shiny app."),
      
      selectInput("ngenes",
                  label = "Select n genes",
                  choices = c("100", "200", "300", "400"),
                  selected = "100")
    ),
    
    mainPanel(DiagrammeROutput("diagram"))
  )
)

shinyApp(ui = ui, server = server)