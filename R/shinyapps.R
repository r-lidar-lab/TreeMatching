#' @rdname lsap_matching
#' @export
lsap_app <- function(treemap)
{
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Tree Matching Explorer"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 3,
          shiny::sliderInput("dxymax", "Max horizontal distance (dxymax)", min = 0, max = 5, value = 2, step = 0.1),
          shiny::sliderInput("dzmax", "Max vertical difference (%) (dzmax)", min = 0, max = 100, value = 30, step = 1),
          shiny::sliderInput("zrel", "Relative importance of Z vs XY (%)", min = 0, max = 100, value = 50, step = 1),
          shiny::sliderInput("unmatch_cost", "Unmatch cost", min = 0, max = 25, value = 5, step = 0.1),
          shiny::plotOutput("histPlot", height = "300px", width = "100%"),
          shiny::actionButton("done", "Done", class = "btn-primary")
        ),
        shiny::mainPanel(
          width = 9,
          shiny::plotOutput("treePlot", height = "900px", width = "100%")
        )
      )
    ),

    server = function(input, output, session) {
      matched <- shiny::reactive({
        match_trees(
          treemap,
          lsap_matching,
          dxymax = input$dxymax,
          dzmax = input$dzmax,
          zrel = input$zrel,
          unmatch_cost = input$unmatch_cost
        )
      })

      output$treePlot <- shiny::renderPlot({
        plot(matched(), scale = 2, gg = TRUE)
      })

      output$histPlot <- shiny::renderPlot({
        guess_unmatch_cost(
          treemap,
          dxymax = input$dxymax,
          dzmax = input$dzmax,
          zrel = input$zrel,
          plot = TRUE
        )
      })

      shiny::observeEvent(input$done, {
        shiny::stopApp(matched())
      })
    }
  )

  return(shiny::runApp(app))
}
