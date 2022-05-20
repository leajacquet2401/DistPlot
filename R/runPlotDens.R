#' @title
#' Shiny App
#'
#'@description
#'This function creates a Shiny App
#'
#' @return
#' Shiny App
#'
#' @export


runPlotDens <- function() {
  require(shiny)
  ui <- fluidPage(
    sliderInput(
      inputId = "range",
      label = "Interval (a,b):",
      min = -5,
      max = 5,
      value = c(a=-1, b=1),
      step = 0.5,
    ),
    sliderInput(
      inputId = "mean",
      label ="Mean - Normal",
      min = -5,
      max=5,
      value=0,
      step = 0.5
    ),
    sliderInput(
      inputId = "std",
      label ="Standard Deviation ('sd) - Normal",
      min = 0.1,
      max=3,
      value=1,
      step = 0.1
    ),
    sliderInput(
      inputId= "degree",
      label = "Degree of freedom ('df') - Student ",
      min = 1,
      max = 50,
      value=1
    ),
    radioButtons(
      inputId = "out",
      label ="Output",
      choices = list("Overlay" = 1, "Normal" = 2, "Student" = 3),
      selected = 1
    ),
    plotOutput("plot")
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      print(PlotDens(input$range[1], input$range[2], input$mean, input$std, input$degree, input$out))
    })
  }
  runApp(shinyApp(ui, server))
}

