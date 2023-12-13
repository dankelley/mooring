# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Run a GUI app for interactive simulations
#'
#' This makes a simple mooring with an anchor, an instrument, and
#' a float. It differs from `app()` in having the instrument,
#' and also by having a pared-down interface. It permits
#' a depth-uniform current, along with a few other current profiles.
#'
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @author Dan Kelley
app2 <- function() {
    if (!requireNamespace("shiny")) {
        stop("must install.packages(\"shiny\") for app2() to work")
    }
    anchorChoices <- anchor("?")
    anchorBuoyancy <- lapply(anchorChoices, \(w) anchor(w)$buoyancy) |> unlist()
    wireChoices <- wire("?")
    wireBuoyancy <- lapply(wireChoices, \(w) wire(w, length = 1)$buoyancy) |> unlist()
    instrumentChoices <- instrument("?")
    instrumentBuoyancy <- lapply(instrumentChoices, \(f) instrument(f)$buoyancy) |> unlist()
    floatChoices <- float("?")
    floatBuoyancy <- lapply(floatChoices, \(f) float(f)$buoyancy) |> unlist()
    dewey1999 <- paste(
        "Dewey, Richard K.",
        "\"Mooring Design & Dynamics-a Matlab",
        "Package for Designing and Analyzing Oceanographic Moorings.\"",
        "Marine Models 1,",
        "no. 1 (December 1, 1999): 103-57.",
        "https://doi.org/10.1016/S1369-9350(00)00002-X"
    )
    dewey2021 <- "Dewey, Richard. \"Mooring Design and Dynamics.\" Accessed May 15, 2021.  http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php"
    indent <- paste0(rep("&nbsp;", 8), collapse = "")
    help <- paste0(
        "Use sliders and pulldown menus to adjust conditions. ",
        "Click the <b>Code</b> button to see code to reproduce the simulation. ",
        "To learn more about the properties of a given float or wire, open an R ",
        "console and type e.g. <br>",
        indent,
        "<tt>float(\"Kiel SFS40in\")</tt><br>or<br>",
        indent,
        "<tt>wire(\"1/4in wire/jack\")</tt><br>A list of float types is obtained with <br>",
        indent, "<tt>float(\"?\")</tt><br>and <br>",
        indent,
        "<tt>wire(\"?\")</tt><br>produces a list of wire types. See Dewey (1999, 2021) for more on these types.<br><b>References</b><br><ul><li>",
        dewey1999, "</li><li>", dewey2021, "</li></ul>"
    )
    ui <- shiny::fluidPage(
        shiny::tags$style(shiny::HTML("body {font-family: 'Arial'; font-size: 10px; margin-left:1ex}")),
        shiny::fluidRow(
            shiny::column(1, shiny::actionButton("help", "Help")),
            shiny::column(1, shiny::actionButton("code", "Code")),
            shiny::column(6, shiny::radioButtons("orderBy", "", choices = c("name", "buoyancy"), selected = "name", inline = TRUE, width = "100%"))
        ),
        shiny::fluidRow(
            shiny::column(4, shiny::sliderInput("waterDepth",
                "Water Depth [m]",
                min = 2.0, max = 300.0, value = 100.0, width = "100%"
            )),
            shiny::column(4, shiny::uiOutput("instrumentDepth")),
            shiny::column(4, shiny::uiOutput("wireLength"))
        ),
        shiny::fluidRow(
            shiny::column(3, shiny::uiOutput("anchorType")),
            shiny::column(3, shiny::uiOutput("wireType")),
            shiny::column(3, shiny::uiOutput("instrumentType")),
            shiny::column(3, shiny::uiOutput("floatType"))
        ),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::selectInput("currentModel", "Current Model",
                    choices = c(
                        "Constant",
                        "Linear",
                        "exp(z/30)",
                        "exp(z/100)",
                        "exp(z/300)"
                    ),
                    selected = "Linear"
                )
            ),
            shiny::column(6, shiny::sliderInput("u",
                "Current [m/s]",
                min = 0.0, max = 5.0, value = 1.0, step = 0.01, width = "100%"
            ))
        ),
        shiny::fluidRow(shiny::column(6, shiny::checkboxGroupInput(
            inputId = "plotChoices", label = NULL,
            choices = c("tension", "shape", "knockdown", "velocity"),
            selected = c("shape", "tension"),
            inline = TRUE, width = "100%"
        ))),
        shiny::fluidRow(shiny::plotOutput("plot"))
    )

    # Server for app, with standard arguments.
    #
    # @param input A list created by the shiny server, with entries for slider settings, etc.
    #
    # @param output A list of output entries, for plotting, etc.
    #
    # @param session A list used for various purposes.
    #
    # @importFrom shiny modalDialog observeEvent renderPlot renderUI showModal stopApp
    #
    # @author Dan Kelley
    server <- function(input, output, session) {
        shiny::observeEvent(input$help, {
            shiny::showModal(shiny::modalDialog(shiny::HTML(help), title = "Using this application", size = "l"))
        })

        shiny::observeEvent(input$code, {
            waterDepth <- input$waterDepth
            wireLength <- input$wireLength
            u <- input$u
            wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
            floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
            msg <- "<pre>library(mooring)<br>"
            msg <- paste0(
                msg,
                sprintf(
                    "m <- mooring(<br>    anchor(depth=%g),<br>    wire(model=\"%s\", length=%g),<br>    float(model=\"%s\"))<br>",
                    waterDepth, wireModel, wireLength, floatModel
                )
            )
            msg <- paste0(msg, "md <- discretise(m, by=1)<br>")
            msg <- paste0(
                msg,
                "mdk <- knockdown(md, u=",
                switch(input$currentModel,
                    "Constant" = sprintf("%g", input$u),
                    "Linear" = sprintf("function(depth) %g*(1-depth/%g)", input$u, input$waterDepth),
                    "exp(z/30)" = sprintf("function(depth) %g*exp(-depth/30)"),
                    "exp(z/100)" = sprintf("function(depth) %g*exp(-depth/100)"),
                    "exp(z/300)" = sprintf("function(depth) %g*exp(-depth/300)")
                ),
                ")<br>"
            )
            msg <- paste0(msg, "par(mfrow=c(1, 2))<br>")
            msg <- paste0(msg, "plot(mdk, which=\"tension\", fancy=TRUE, showDepths=FALSE)<br>")
            msg <- paste0(msg, "plot(mdk, which=\"shape\", fancy=TRUE)<br>")
            msg <- paste0(msg, "</pre>")
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title = "R code", size = "l"))
        })

        output$instrumentDepth <- shiny::renderUI({
            value <- if (input$waterDepth > 10) input$waterDepth - 10 else input$waterDepth / 2
            shiny::sliderInput("instrumentDepth", "Instrument Depth [m]",
                min = 0.5, max = input$waterDepth, value = value, step = 0.1, width = "100%"
            )
        })

        output$wireLength <- shiny::renderUI({
            value <- if (input$waterDepth > 10) input$waterDepth - 10 else input$waterDepth
            shiny::sliderInput("wireLength", "Wire Length [m]",
                min = 0.5, max = input$waterDepth, value = value, step = 0.1, width = "100%"
            )
        })

        output$anchorType <- shiny::renderUI({
            o <- if (input$orderBy == "name") seq_along(anchorChoices) else order(anchorBuoyancy)
            shiny::selectInput("anchorModel", "Anchor Type",
                choices = paste0(anchorChoices[o], " [", anchorBuoyancy[o], "kg]"),
                selected = anchorChoices[o[1]],
                width = "100%"
            )
        })

        output$wireType <- shiny::renderUI({
            o <- if (input$orderBy == "name") seq_along(wireChoices) else order(wireBuoyancy)
            shiny::selectInput("wireModel", "Wire Type",
                choices = paste0(wireChoices[o], " [", wireBuoyancy[o], "kg/m]"),
                selected = wireChoices[o[1]],
                width = "100%"
            )
        })

        output$instrumentType <- shiny::renderUI({
            o <- if (input$orderBy == "name") seq_along(instrumentChoices) else order(instrumentBuoyancy)
            shiny::selectInput("instrumentModel", "instrument Type",
                choices = paste0(instrumentChoices[o], " [", instrumentBuoyancy[o], "kg]"),
                selected = instrumentChoices[o[1]],
                width = "100%"
            )
        })

        output$floatType <- shiny::renderUI({
            o <- if (input$orderBy == "name") seq_along(floatChoices) else order(floatBuoyancy)
            shiny::selectInput("floatModel", "Float Type",
                choices = paste0(floatChoices[o], " [", floatBuoyancy[o], "kg]"),
                selected = floatChoices[o[1]],
                width = "100%"
            )
        })

        output$plot <- shiny::renderPlot(
            {
                waterDepth <- input$waterDepth
                wireLength <- input$wireLength
                if (!is.null(wireLength)) { # undefined at the start, since it depends on another slider
                    u <- input$u
                    anchorModel <- gsub("[ ]+\\[.*kg\\]$", "", input$anchorModel)
                    wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
                    floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
                    instrumentModel <- gsub("[ ]+\\[.*kg\\]$", "", input$instrumentModel)
                    #> message("currentModel='", input$currentModel, "'")
                    #> message("waterDepth=", waterDepth)
                    #> message("  wireLength=", wireLength)
                    #> message("  u=", u)
                    #> message("  wireModel=", wireModel)
                    #> message("  floatModel=", floatModel)
                    # FIXME: add instrument here
                    m <- mooring(
                        anchor(anchorModel, depth = waterDepth),
                        wire(model = wireModel, length = wireLength),
                        float(model = floatModel)
                    )
                    md <- discretise(m, 1)
                    u <- switch(input$currentModel,
                        "Constant" = input$u,
                        "Linear" = function(depth) input$u * (1 - depth / waterDepth),
                        "exp(z/30)" = function(depth) input$u * exp(-depth / 30),
                        "exp(z/100)" = function(depth) input$u * exp(-depth / 100),
                        "exp(z/300)" = function(depth) input$u * exp(-depth / 300)
                    )
                    mdk <- knockdown(md, u)
                    mar <- c(2.5, 2.5, 0.5, 0.5)
                    mpg <- c(1.5, 0.5, 0)
                    cex <- 1.2
                    nchoices <- length(input$plotChoices)
                    if (nchoices == 1) {
                        par(mfrow = c(1, 1), mar = mar, mgp = mpg, cex = cex)
                    } else if (nchoices == 2) {
                        par(mfrow = c(1, 2), mar = mar, mgp = mpg, cex = cex)
                    } else if (nchoices == 3) {
                        par(mfrow = c(1, 3), mar = mar, mgp = mpg, cex = cex)
                    } else if (nchoices == 4) {
                        par(mfrow = c(2, 2), mar = mar, mgp = mpg, cex = cex)
                    }
                    for (choice in input$plotChoices) {
                        plot(mdk, which = choice, fancy = TRUE, showDepths = FALSE)
                    }
                }
            },
            pointsize = 16, # this has no effect
            height = 500
        ) # , height=500)
    }
    shiny::shinyApp(ui = ui, server = server)
}
