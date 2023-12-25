# vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

#' Run a two-element GUI app for interactive simulations
#'
#' This makes a simple mooring with an anchor, an instrument, and
#' a float, in a variety of current profiles. It differs from [app1()]
#' in having the instrument, and in having some interface changes.
#'
#' @param debug logical value indicating whether to print debugging
#' output.
#'
#' @export
#'
#' @family interactive apps
#'
#' @author Dan Kelley
app2 <- function(debug = FALSE) {
    dmsg <- function(...) {
        if (debugMode) message(...)
    }
    if (!requireNamespace("shiny")) {
        stop("must install.packages(\"shiny\") for app2() to work")
    }
    debugMode <- debug
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
        "Type 'n' for a nearshore example, 's' for a shelf example, or 'd' for a deep example.",
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
        shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
        shiny::tags$style(shiny::HTML("body {font-family: 'Arial'; font-size: 10px; margin-left:1ex}")),
        shiny::fluidRow(
            shiny::column(2, shiny::selectInput("preset", "Preset", choices = c("Nearshore", "Shelf", "Deep"), selected = "Shelf")),
            shiny::column(1, shiny::actionButton("code", "Code")),
            shiny::column(1, shiny::actionButton("help", "Help"))
            # shiny::column(6, shiny::radioButtons("orderBy", "Order Components By", choices = c("name", "buoyancy"), selected = "name", inline = TRUE, width = "100%"))
        ),
        shiny::fluidRow(
            shiny::column(4, shiny::uiOutput("waterDepth")),
            shiny::column(4, shiny::uiOutput("wireLength")),
            shiny::column(4, shiny::uiOutput("instrumentDepth"))
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
            shiny::column(6, shiny::sliderInput("u", "Current [m/s]",
                min = 0.0, max = 1.0, value = 0.5, step = 0.01, width = "100%"
            ))
        ),
        shiny::fluidRow(shiny::column(6, shiny::checkboxGroupInput(
            inputId = "plotChoices", label = NULL,
            choices = c("shape", "velocity", "knockdown", "tension"),
            selected = c("shape", "velocity"),
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

        shiny::observeEvent(input$keypressTrigger, {
            key <- intToUtf8(input$keypress)
            ## dmsg("key='",key, "'\n", sep="")
            if (key == "d") {
                debugMode <<- !debugMode
                message("'d' pressed, setting new debug mode to ", debugMode)
            } else if (key == "?") {
                shiny::showModal(shiny::modalDialog(
                    titl6 = "Key-stroke commands",
                    shiny::HTML("<ul>
                        <li> '<b>d</b>': toggle debugging mode</b>oastal mooring</li>
                        <li> '<b>?</b>': display this message</li>
                        </ul>"),
                    easyClose = TRUE
                ))
            }
        })

        shiny::observeEvent(input$preset, {
            if (identical(input$preset, "Nearshore")) {
                dmsg("Preset: 'Nearshore'")
                shiny::updateSliderInput(session, inputId = "waterDepth", min = 1.0, max = 5.0, value = 2.0, step = 0.1)
                shiny::updateSliderInput(session, inputId = "wireLength", min = 0.5, max = 1.6, value = 1.0, step = 0.1)
                shiny::updateSliderInput(session, inputId = "instrumentDepth", min = 1.0, max = 5.0, value = 1.0)
                shiny::updateSliderInput(session, inputId = "u", min = 0.0, max = 2.0, value = 1, step = 0.01)
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "Linear")
                wire <- "3/8in leaded polypropylene"
                shiny::updateSelectInput(session,
                    inputId = "wireModel",
                    selected = paste0(wire, " [", wire(wire, length = 1)$buoyancy, "kg/m]")
                )
                anchor <- "2 rotor"
                shiny::updateSelectInput(session, inputId = "anchorModel",
                    selected = paste0(anchor, " [", anchor(anchor)$buoyancy, "kg]")
                )
                instrument <- "Hobo Temp U22"
                shiny::updateSelectInput(session, inputId = "instrumentModel",
                    selected = paste0(instrument, " [", instrument(instrument)$buoyancy, "kg]")
                )
                float <- "11in centre hole tfloat"
                shiny::updateSelectInput(session, inputId = "floatModel",
                    selected = paste0(float, " [", float(float)$buoyancy, "kg]")
                )
            } else if (identical(input$preset, "Shelf")) {
                dmsg("Preset: 'Shelf'")
                shiny::updateSliderInput(session, inputId = "waterDepth", min = 2.0, max = 200.0, value = 100.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "wireLength", min = 10.0, max = 180.0, value = 90.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "instrumentDepth", min = 10.0, max = 160.0, value = 50.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "u", min = 0.0, max = 1.0, value = 0.5)
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "Linear")
                wire <- "1/4in wire/jack"
                shiny::updateSelectInput(session, inputId = "wireModel",
                    choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                    selected = paste0(wire, " [", wire(wire, length = 1)$buoyancy, "kg/m]")
                )
                anchor <- "1 Railway Wheel"
                shiny::updateSelectInput(session, inputId = "anchorModel",
                    # choices = paste0(anchorChoices, " [", anchorBuoyancy, "kg]"),
                    selected = paste0(anchor, " [", anchor(anchor)$buoyancy, "kg]")
                )
                instrument <- "seabird CTD (ios oxygen with bar)"
                shiny::updateSelectInput(session, inputId = "instrumentModel",
                    # choices = paste0(instrumentChoices, " [", instrumentBuoyancy, "kg]"),
                    # selected = "Hobo Temp U22 [0.0133kg]"
                    selected = paste0(instrument, " [", instrument(instrument)$buoyancy, "kg]")
                )
                float <- "BUB 2x17in glass"
                shiny::updateSelectInput(session, inputId = "floatModel",
                    selected = paste0(float, " [", float(float)$buoyancy, "kg]")
                )
            } else if (identical(input$preset, "Deep")) {
                dmsg("Preset: 'Deep'")
                shiny::updateSliderInput(session, inputId = "waterDepth", min = 100.0, max = 1500.0, value = 1000.0, step = 10.0)
                shiny::updateSliderInput(session, inputId = "wireLength", min = 100.0, max = 1480.0, value = 980.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "instrumentDepth", min = 100.0, max = 1470.0, value = 500.0, step = 10.0)
                shiny::updateSliderInput(session, inputId = "u", min = 0.0, max = 1.0, value = 0.5)
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "Linear")
                wire <- "1/4in wire/jack"
                shiny::updateSelectInput(session, inputId = "wireModel",
                    choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                    selected = paste0(wire, " [", wire(wire, length = 1)$buoyancy, "kg/m]")
                )
                anchor <- "1 Railway Wheel"
                shiny::updateSelectInput(session, inputId = "anchorModel",
                    selected = paste0(anchor, " [", anchor(anchor)$buoyancy, "kg]")
                )
                instrument <- "seabird CTD (ios oxygen with bar)"
                shiny::updateSelectInput(session, inputId = "instrumentModel",
                    selected = paste0(instrument, " [", instrument(instrument)$buoyancy, "kg]")
                )
                float <- "30in float"
                shiny::updateSelectInput(session, inputId = "floatModel",
                    selected = paste0(float, " [", float(float)$buoyancy, "kg]")
                )
            } else {
                stop("How can we get here? (Programming error.)")
            }
        })

        shiny::observeEvent(input$code, {
            wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
            waterDepth <- input$waterDepth
            wireBelow <- input$waterDepth - input$instrumentDepth
            instrumentModel <- gsub("[ ]+\\[.*kg\\]$", "", input$instrumentModel)
            wireAbove <- input$wireLength - wireBelow
            floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
            u <- input$u
            msg <- "<pre>library(mooring)<br>"
            msg <- paste0(msg, "# See help pages and vignettes for more details<br>")
            msg <- paste0(msg, sprintf("m <- mooring(<br>    anchor(model = \"%s\", depth = %g),<br>", gsub(" \\[.*$", "", input$anchorModel), input$waterDepth))
            msg <- paste0(msg, sprintf("    wire(model = \"%s\", length = %g),<br>", wireModel, wireBelow))
            msg <- paste0(msg, sprintf("    clamped(instrument(model = \"%s\")),<br>", instrumentModel))
            msg <- paste0(msg, sprintf("    wire(model = \"%s\", length = %g),<br>", wireModel, wireAbove))
            msg <- paste0(msg, sprintf("    float(model = \"%s\")<br>", floatModel))
            msg <- paste0(msg, ")<br>")
            msg <- paste0(msg, "md <- discretise(m, by = 1)<br>")
            msg <- paste0(
                msg,
                "mdk <- knockdown(md, u = ",
                switch(input$currentModel,
                    "Constant" = sprintf("%g", input$u),
                    "Linear" = sprintf("function(depth) %g * (1 - depth / %g)", input$u, input$waterDepth),
                    "exp(z/30)" = sprintf("function(depth) %g * exp(-depth / 30)", input$u),
                    "exp(z/100)" = sprintf("function(depth) %g * exp(-depth / 100)", input$u),
                    "exp(z/300)" = sprintf("function(depth) %g * exp(-depth / 300)", input$u)
                ),
                ")<br>"
            )
            msg <- paste0(msg, "# Demonstrate all 4 plot types (unlike the app)<br>")
            msg <- paste0(msg, "par(mfrow = c(2, 2))<br>")
            msg <- paste0(msg, "plot(mdk, which = \"tension\", fancy = TRUE, showDepths = FALSE)<br>")
            msg <- paste0(msg, "plot(mdk, which = \"shape\", fancy = TRUE)<br>")
            msg <- paste0(msg, "plot(mdk, which = \"knockdown\", fancy = TRUE)<br>")
            msg <- paste0(msg, "plot(mdk, which = \"velocity\", fancy = TRUE)<br>")
            msg <- paste0(msg, "</pre>")
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title = "R code", size = "l"))
        })


        output$waterDepth <- shiny::renderUI({
            shiny::sliderInput("waterDepth", "Water Depth [m]",
                min = 2.0, max = 200.0, value = 100, step = 1.0, width = "100%"
            )
        })

        # FIXME: observe waterDepth and alter wireLength to fit
        # FIXME: observe wireDepth and alter instrumentDepth to fit

        output$wireLength <- shiny::renderUI({
            shiny::sliderInput("wireLength", "Wire Length [m]",
                min = 10.0, max = 180.0, value = 90.0, step = 1.0, width = "100%"
            )
        })

        output$instrumentDepth <- shiny::renderUI({
            #. # instrumentDepth must be between waterDepth and (waterDepth-wireLength); make
            #. # the default be the midpoint. If neither defined yet (that's how shiny works
            #. # just use some values that will get replaced a fraction of a second later)
            #. if (is.null(input$waterDepth) || is.null(input$wireLength)) {
            #.     depthMax <- 100
            #.     depthMin <- 0
            #. } else {
            #.     depthMax <- input$waterDepth
            #.     depthMin <- input$waterDepth - input$wireLength
            #. }
            #. value <- 0.5 * (depthMin + depthMax)
            #. message("waterDepth=", input$waterDepth, ", wireLength=", input$wireLength)
            #. message("value=", value, ", depthMin=", depthMin, ", depthMax=", depthMax)
            #. # value <- if (input$waterDepth > 10) input$waterDepth - 10 else input$waterDepth / 2
            #. shiny::sliderInput("instrumentDepth", "Instrument Depth [m]",
            #.     min = depthMin, max = depthMax, value = value, step = 0.1, width = "100%"
            #. )
            shiny::sliderInput("instrumentDepth", "Instrument Depth [m]",
                min = 10.0, max = 160.0, value = 50.0, step = 1.0, width = "100%"
            )
        })

        output$anchorType <- shiny::renderUI({
            anchor <- "1 Railway Wheel"
            shiny::selectInput("anchorModel", "Anchor Type",
                choices = paste0(anchorChoices, " [", anchorBuoyancy, "kg]"),
                selected = paste0(anchor, " [", anchor(anchor)$buoyancy, "kg]"),
                width = "100%"
            )
        })

        output$wireType <- shiny::renderUI({
            wire <- "1/4in wire/jack"
            shiny::selectInput("wireModel", "Wire Type",
                choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                selected = paste0(wire, " [", wire(wire, length = 1)$buoyancy, "kg/m]"),
                width = "100%"
            )
        })

        output$instrumentType <- shiny::renderUI({
            instrument <- "seabird CTD (ios oxygen with bar)"
            shiny::selectInput("instrumentModel", "instrument Type",
                choices = paste0(instrumentChoices, " [", instrumentBuoyancy, "kg]"),
                selected = paste0(instrument, " [", instrument(instrument)$buoyancy, "kg]"),
                width = "100%"
            )
        })

        output$floatType <- shiny::renderUI({
            float <- "BUB 2x17in glass"
            shiny::selectInput("floatModel", "Float Type",
                choices = paste0(floatChoices, " [", floatBuoyancy, "kg]"),
                selected = paste0(float, " [", float(float)$buoyancy, "kg]"),
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
                    dmsg("    anchorModel='", anchorModel, "' (height=", anchor(anchorModel)$height, "m)")
                    wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
                    dmsg("    wireModel='", wireModel, "'")
                    floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
                    dmsg("    floatModel='", floatModel, "' (height=", float(floatModel)$height, "m)")
                    instrumentModel <- gsub("[ ]+\\[.*kg\\]$", "", input$instrumentModel)
                    dmsg("    instrumentModel='", instrumentModel, "'")
                    wireBelow <- waterDepth - input$instrumentDepth
                    dmsg("    input$wireLength=", input$wireLength)
                    if (wireBelow < 0.1) wireBelow <- 0.1
                    dmsg("    wireBelow=", wireBelow)
                    wireAbove <- input$wireLength - wireBelow
                    if (wireAbove < 0.1) wireAbove <- 0.1
                    dmsg("    wireAbove=", wireAbove)
                    m <- mooring(
                        anchor(anchorModel, depth = waterDepth),
                        wire(model = wireModel, length = wireBelow),
                        clamped(instrument(instrumentModel)),
                        wire(model = wireModel, length = wireAbove),
                        float(model = floatModel)
                    )
                    #message(str(m))
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
