# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(shiny)
library(bslib)

ui <- page_sidebar(
    title = "Two-element Mooring",
    sidebar = sidebar(
        title = "Controls",
        accordion(
            open = FALSE,
            accordion_panel(
                "Plot Title",
                selectInput(
                    "title", "Plot Title",
                    c("histogram", "HISTOGRAM")
                )
            ),
            accordion_panel(
                "Plot Details",
                numericInput("breaks", "Number of breaks", 30),
                sliderInput("depth", "Water Depth [m]", 0, 100, 50)
            )
        )
    ),
    card(
        #card_header("Histogram"),
        plotOutput("hist")
    )
)

server <- function(input, output) {
    output$hist <- renderPlot({
        hist(volcano, breaks = input$breaks, main = input$title)
        mtext(paste("Water Depth =", input$depth), col = 2, font = 2)
    })
}

shinyApp(ui, server)
