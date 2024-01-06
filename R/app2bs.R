# vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

#' Run a two-element GUI app for interactive simulations
#'
#' This makes a simple mooring with an anchor, an instrument, and
#' a float, in a variety of current profiles. It differs from [app1()]
#' in having the instrument, and in having some interface changes.
#' It differs from [app2()] in using the 'bslib' GUI framework,
#' which is cleaner than the framework used in [app1()] and [app2()].
#'
#' @param debug logical value indicating whether to print debugging
#' output.
#'
#' @export
#' @importFrom bslib accordion accordion_panel card sidebar
#'
#' @family interactive apps
#'
#' @author Dan Kelley
app2bs <- function(debug = FALSE) {
    dmsg <- function(...) {
        if (debugMode) message(...)
    }
    if (!requireNamespace("shiny")) {
        stop("must install.packages(\"shiny\") for app2() to work")
    }
    debugMode <- debug
    anchorChoices <- anchor("?")
    anchorBuoyancy <- lapply(anchorChoices, \(w) anchor(w)@buoyancy) |> unlist()
    wireChoices <- wire("?")
    wireBuoyancy <- lapply(wireChoices, \(w) wire(w, length = 1)@buoyancy) |> unlist()
    instrumentChoices <- instrument("?")
    instrumentBuoyancy <- lapply(instrumentChoices, \(f) instrument(f)@buoyancy) |> unlist()
    floatChoices <- float("?")
    floatBuoyancy <- lapply(floatChoices, \(f) float(f)@buoyancy) |> unlist()
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
    ui <- bslib::page_sidebar(
        # shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
        # shiny::tags$style(shiny::HTML("body {font-family: 'Arial'; font-size: 10px; margin-left:1ex}")),
        title = "app2bs",
        sidebar = bslib::sidebar(
            title = "Controls",
            bslib::accordion(
                open = c("Depths", "Elements"),
                multiple = TRUE,
                bslib::accordion_panel(
                    "Presets",
                    shiny::selectInput("preset", "Preset", choices = c("Nearshore", "Shelf", "Deep"), selected = "Shelf"),
                    shiny::actionButton("code", "Code"),
                    shiny::actionButton("help", "Help")
                ),
                bslib::accordion_panel(
                    "Depths",
                    shiny::uiOutput("waterDepth"),
                    shiny::uiOutput("wireLength"),
                    shiny::uiOutput("instrumentDepth")
                ),
                bslib::accordion_panel(
                    "Elements",
                    shiny::uiOutput("anchorType"),
                    shiny::uiOutput("wireType"),
                    shiny::uiOutput("instrumentType"),
                    shiny::uiOutput("floatType")
                ),
                bslib::accordion_panel(
                    "Current",
                    shiny::selectInput("currentModel", "Current Model",
                        choices = c(
                            "Constant",
                            "Linear",
                            "exp(-depth/30)",
                            "exp(-depth/100)",
                            "exp(-depth/300)"
                        ),
                        selected = "Linear"
                    ),
                    shiny::sliderInput("u", "Current [m/s]",
                        min = 0.0, max = 1.0, value = 0.5, step = 0.01, width = "100%"
                    )
                ),
                bslib::accordion_panel(
                    "Plot Choice",
                    shiny::checkboxGroupInput(
                        inputId = "plotChoices", label = NULL,
                        choices = c("shape", "velocity", "knockdown", "tension"),
                        selected = c("shape", "velocity"),
                        inline = TRUE, width = "100%"
                    )
                ),
            )
        ),
        bslib::card(
            shiny::plotOutput("plot")
        )
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
            # dmsg("key='",key, "'\n", sep="")
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
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "Constant")
                wire <- "3/8in leaded polypropylene"
                shiny::updateSelectInput(session,
                    inputId = "wireType",
                    selected = paste0(wire, " [", wire(wire, length = 1)@buoyancy, "kg/m]")
                )
                anchor <- "2 rotor"
                shiny::updateSelectInput(session,
                    inputId = "anchorType",
                    selected = paste0(anchor, " [", anchor(anchor)@buoyancy, "kg]")
                )
                instrument <- "Hobo Temp U22"
                shiny::updateSelectInput(session,
                    inputId = "instrumentType",
                    selected = paste0(instrument, " [", instrument(instrument)@buoyancy, "kg]")
                )
                float <- "11in centre hole tfloat"
                shiny::updateSelectInput(session,
                    inputId = "floatType",
                    selected = paste0(float, " [", float(float)@buoyancy, "kg]")
                )
            } else if (identical(input$preset, "Shelf")) {
                dmsg("Preset: 'Shelf'")
                shiny::updateSliderInput(session, inputId = "waterDepth", min = 2.0, max = 200.0, value = 150.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "wireLength", min = 10.0, max = 180.0, value = 130.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "instrumentDepth", min = 10.0, max = 160.0, value = 75.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "u", min = 0.0, max = 1.0, value = 0.25)
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "exp(-depth/30)")
                wire <- "1/4in wire/jack"
                shiny::updateSelectInput(session,
                    inputId = "wireType",
                    choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                    selected = paste0(wire, " [", wire(wire, length = 1)@buoyancy, "kg/m]")
                )
                anchor <- "1 Railway Wheel"
                shiny::updateSelectInput(session,
                    inputId = "anchorType",
                    selected = paste0(anchor, " [", anchor(anchor)@buoyancy, "kg]")
                )
                instrument <- "seabird CTD (ios oxygen with bar)"
                shiny::updateSelectInput(session,
                    inputId = "instrumentType",
                    selected = paste0(instrument, " [", instrument(instrument)@buoyancy, "kg]")
                )
                float <- "BUB 2x17in glass"
                shiny::updateSelectInput(session,
                    inputId = "floatType",
                    selected = paste0(float, " [", float(float)@buoyancy, "kg]")
                )
            } else if (identical(input$preset, "Deep")) {
                dmsg("Preset: 'Deep'")
                shiny::updateSliderInput(session, inputId = "waterDepth", min = 100.0, max = 1500.0, value = 1000.0, step = 10.0)
                shiny::updateSliderInput(session, inputId = "wireLength", min = 100.0, max = 1480.0, value = 980.0, step = 1.0)
                shiny::updateSliderInput(session, inputId = "instrumentDepth", min = 100.0, max = 1470.0, value = 500.0, step = 10.0)
                shiny::updateSliderInput(session, inputId = "u", min = 0.0, max = 1.0, value = 0.5)
                shiny::updateSelectInput(session, inputId = "currentModel", selected = "Linear")
                wire <- "1/4in wire/jack"
                shiny::updateSelectInput(session,
                    inputId = "wireType",
                    choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                    selected = paste0(wire, " [", wire(wire, length = 1)@buoyancy, "kg/m]")
                )
                anchor <- "1 Railway Wheel"
                shiny::updateSelectInput(session,
                    inputId = "anchorType",
                    selected = paste0(anchor, " [", anchor(anchor)@buoyancy, "kg]")
                )
                instrument <- "seabird CTD (ios oxygen with bar)"
                shiny::updateSelectInput(session,
                    inputId = "instrumentType",
                    selected = paste0(instrument, " [", instrument(instrument)@buoyancy, "kg]")
                )
                float <- "30in float"
                shiny::updateSelectInput(session,
                    inputId = "floatType",
                    selected = paste0(float, " [", float(float)@buoyancy, "kg]")
                )
            } else {
                stop("How can we get here? (Programming error.)")
            }
        })

        shiny::observeEvent(input$code, {
            wireType <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireType)
            waterDepth <- input$waterDepth
            wireBelow <- input$waterDepth - input$instrumentDepth
            instrumentType <- gsub("[ ]+\\[.*kg\\]$", "", input$instrumentType)
            wireAbove <- input$wireLength - wireBelow
            #message("input$wireLength=",input$wireLength,", wireBelow=", wireBelow,", wireAbove=",wireAbove)
            floatType <- gsub("[ ]+\\[.*kg\\]$", "", input$floatType)
            u <- input$u
            msg <- "<pre>library(mooring)<br>"
            msg <- paste0(msg, "# See help pages and vignettes for more details<br>")
            msg <- paste0(msg, sprintf("m <- mooring(<br>    anchor(model = \"%s\"),<br>", gsub(" \\[.*$", "", input$anchorType)))
            msg <- paste0(msg, sprintf("    wire(model = \"%s\", length = %g),<br>", wireType, wireBelow))
            msg <- paste0(msg, sprintf("    clamped(instrument(model = \"%s\")),<br>", instrumentType))
            msg <- paste0(msg, sprintf("    wire(model = \"%s\", length = %g),<br>", wireType, wireAbove))
            msg <- paste0(msg, sprintf("    float(model = \"%s\"),<br>", floatType))
            msg <- paste0(msg, sprintf("    waterDepth = %g<br>", input$waterDepth))
            #msg <- paste0(msg, sprintf("# wireAbove=%f wireBelow=%f<br>\n", wireAbove, wireBelow))
            msg <- paste0(msg, ")<br>")
            msg <- paste0(msg, "ms <- segmentize(m, by = ", max(1, round(input$waterDepth / 100)), ")<br>")
            msg <- paste0(
                msg,
                "msk <- knockdown(ms, u = ",
                switch(input$currentModel,
                    "Constant" = sprintf("%g", input$u),
                    "Linear" = sprintf("function(depth) %g * (1 - depth / %g)", input$u, input$waterDepth),
                    "exp(-depth/30)" = sprintf("function(depth) %g * exp(-depth / 30)", input$u),
                    "exp(-depth/100)" = sprintf("function(depth) %g * exp(-depth / 100)", input$u),
                    "exp(-depth/300)" = sprintf("function(depth) %g * exp(-depth / 300)", input$u)
                ),
                ")<br>"
            )
            msg <- paste0(msg, "# Demonstrate all 4 plot types (unlike the app)<br>")
            msg <- paste0(msg, "par(mfrow = c(2, 2))<br>")
            msg <- paste0(msg, "draw(msk, which = \"tension\", fancy = TRUE, showDepths = FALSE)<br>")
            msg <- paste0(msg, "draw(msk, which = \"shape\", fancy = TRUE)<br>")
            msg <- paste0(msg, "draw(msk, which = \"knockdown\", fancy = TRUE)<br>")
            msg <- paste0(msg, "draw(msk, which = \"velocity\", fancy = TRUE)<br>")
            msg <- paste0(msg, "</pre>")
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title = "R code", size = "l"))
        })

        output$waterDepth <- shiny::renderUI({
            dmsg("creating waterDepth GUI element ...")
            shiny::sliderInput("waterDepth", "Water Depth [m]",
                min = 2.0, max = 200.0, value = 100, step = 1.0, width = "100%"
            )
            #<> # next is cleaner, but it lets you enter *any* value, even e.g. negatives
            #<>shiny::numericInput("waterDepth", "Water Depth [m]",
            #<>    min = 2.0, max = 200.0, value = 100, step = 1.0
            #<>)
            # dmsg("    ... done")
        })

        output$wireLength <- shiny::renderUI({
            dmsg("creating wireLength GUI element ...")
            shiny::sliderInput("wireLength", "Wire Length [m]",
                min = 10.0, max = 180.0, value = 90.0, step = 1.0, width = "100%"
            )
            # dmsg("    ... done")
        })

        output$instrumentDepth <- shiny::renderUI({
            dmsg("creating instrumentDepth GUI element ...")
            shiny::sliderInput("instrumentDepth", "Instrument Depth [m]",
                min = 10.0, max = 160.0, value = 50.0, step = 1.0, width = "100%"
            )
            # dmsg("    ...")
        })

        output$anchorType <- shiny::renderUI({
            dmsg("creating anchorType GUI element ...")
            anchor <- "1 Railway Wheel"
            shiny::selectInput("anchorType", "Anchor Type",
                choices = paste0(anchorChoices, " [", anchorBuoyancy, "kg]"),
                selected = paste0(anchor, " [", anchor(anchor)@buoyancy, "kg]"),
                width = "100%"
            )
            # dmsg("    ...")
        })

        output$wireType <- shiny::renderUI({
            dmsg("creating wireType GUI element ...")
            wire <- "1/4in wire/jack"
            shiny::selectInput("wireType", "Wire Type",
                choices = paste0(wireChoices, " [", wireBuoyancy, "kg/m]"),
                selected = paste0(wire, " [", wire(wire, length = 1)@buoyancy, "kg/m]"),
                width = "100%"
            )
            # dmsg("    ... done")
        })

        output$instrumentType <- shiny::renderUI({
            dmsg("creating instrumentType GUI element ...")
            instrument <- "seabird CTD (ios oxygen with bar)"
            shiny::selectInput("instrumentType", "instrument Type",
                choices = paste0(instrumentChoices, " [", instrumentBuoyancy, "kg]"),
                selected = paste0(instrument, " [", instrument(instrument)@buoyancy, "kg]"),
                width = "100%"
            )
            # dmsg("    ... done")
        })

        output$floatType <- shiny::renderUI({
            dmsg("creating floatType GUI element ...")
            float <- "BUB 2x17in glass"
            shiny::selectInput("floatType", "Float Type",
                choices = paste0(floatChoices, " [", floatBuoyancy, "kg]"),
                selected = paste0(float, " [", float(float)@buoyancy, "kg]"),
                width = "100%"
            )
            # dmsg("    ... done")
        })

        output$plot <- shiny::renderPlot(
            {
                dmsg("creating plot ...")
                dmsg("    input$waterDepth=", input$waterDepth)
                dmsg("    input$wireLength=", input$wireLength)
                dmsg("    input$u=", input$u)
                dmsg("    input$currentModel=", input$currentModel)
                dmsg("    input$anchorType=", input$anchorType)
                dmsg("    input$wireType=", input$wireType)
                dmsg("    input$instrumentType=", input$instrumentType)
                dmsg("    input$floatType=", input$floatType)
                # cannot plot until all GUI elements are defined (need this because
                # shiny calls all elements at the start, as it builds up the interface)
                canPlot <- !is.null(input$waterDepth) && !is.null(input$wireLength) && !is.null(input$anchorType) && !is.null(input$instrumentType) && !is.null(input$floatType)
                if (canPlot) {
                    waterDepth <- input$waterDepth
                    wireLength <- input$wireLength
                    u <- input$u
                    anchorType <- gsub("[ ]+\\[.*kg\\]$", "", input$anchorType)
                    wireType <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireType)
                    floatType <- gsub("[ ]+\\[.*kg\\]$", "", input$floatType)
                    instrumentType <- gsub("[ ]+\\[.*kg\\]$", "", input$instrumentType)
                    wireBelow <- waterDepth - input$instrumentDepth
                    if (wireBelow < 0.1) wireBelow <- 0.1
                    dmsg("    wireBelow=", wireBelow)
                    wireAbove <- wireLength - wireBelow
                    if (wireAbove < 0.1) wireAbove <- 0.1
                    dmsg("    wireAbove=", wireAbove)
                    m <- mooring(
                        anchor(anchorType),
                        wire(model = wireType, length = wireBelow),
                        clamped(instrument(instrumentType)),
                        wire(model = wireType, length = wireAbove),
                        float(model = floatType),
                        waterDepth = waterDepth
                    )
                    # message(str(m))
                    ms <- segmentize(m, max(1, round(input$waterDepth / 100)))
                    u <- switch(input$currentModel,
                        "Constant" = input$u,
                        "Linear" = function(depth) input$u * (1 - depth / waterDepth),
                        "exp(-depth/30)" = function(depth) input$u * exp(-depth / 30),
                        "exp(-depth/100)" = function(depth) input$u * exp(-depth / 100),
                        "exp(-depth/300)" = function(depth) input$u * exp(-depth / 300)
                    )
                    msk <- knockdown(ms, u, debug = debug)
                    attr <- attributes(msk)
                    mar <- c(0.5, 2.5, 3.75, 0.5)
                    mgp <- c(1.5, 0.5, 0)
                    cex <- 1.2
                    nchoices <- length(input$plotChoices)
                    if (nchoices == 1) {
                        par(mfrow = c(1, 1), cex = cex)
                    } else if (nchoices == 2) {
                        par(mfrow = c(1, 2), cex = cex)
                    } else if (nchoices == 3) {
                        par(mfrow = c(1, 3), cex = cex)
                    } else if (nchoices == 4) {
                        par(mfrow = c(2, 2), cex = cex)
                    }
                    ylim <- NULL # c(waterDepth * 0.95, 0.05 * waterDepth)
                    titleShown <- FALSE
                    for (choice in input$plotChoices) {
                        if (is.null(ylim)) {
                            draw(msk,
                                which = choice, mar = mar, mgp = mgp, fancy = TRUE, showDepths = FALSE,
                                xaxs = "r", yaxs = "r"
                            )
                            ylim <- par("usr")[3:4]
                        } else {
                            draw(msk,
                                which = choice, mar = mar, mgp = mgp, fancy = TRUE, showDepths = FALSE,
                                ylim = ylim, xaxs = "r", yaxs = "r"
                                # ylim = ylim, yaxs = "r"
                            )
                        }
                        if (!titleShown) {
                            mtext(
                                sprintf(
                                    "Converged to %.03fm and %.02fdeg in %s",
                                    attr$RMSChangeDepth,
                                    attr$RMSChangeAngle,
                                    pluralize("iteration", n = attr$iteration)
                                ),
                                cex = par("cex"),
                                col = 2,
                                font = 2,
                                line = 2.75
                            )
                            titleShown <- TRUE
                        }
                    }
                } else {
                    dmsg("cannot plut until more GUI elements are defined")
                }
            },
            pointsize = 14 # ,
            # height = 500
        )
    }
    shiny::shinyApp(ui = ui, server = server)
}
