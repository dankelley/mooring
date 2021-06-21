## app for simulating the effect of a vessel strike on a whale

#' Run a GUI app for interactive simulations
#'
#' This makes a simple mooring with an anchor, a line, and a float, in
#' a depth-uniform current.  Sliders adjust line length and current speed.
#' Pulldown menus adjust wire and float types.
#'
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @author Dan Kelley
app <- function()
{
    if (!requireNamespace("shiny"))
        stop("must install.packages(\"shiny\") for app() to work")
    floatChoices <- float("?")
    floatBuoyancy <- unlist(lapply(floatChoices, function(f) float(f)$buoyancy))
    wireChoices <- wire("?")
    wireBuoyancy <- unlist(lapply(wireChoices, function(w) wire(w, length=1)$buoyancyPerMeter))
    dewey1999 <- "Dewey, Richard K. \"Mooring Design & Dynamics-a Matlab\" Package for Designing and Analyzing Oceanographic Moorings.\" Marine Models 1, no. 1 (December 1, 1999): 103-57. https://doi.org/10.1016/S1369-9350(00)00002-X"
    dewey2021 <- "Dewey, Richard. \"Mooring Design and Dynamics.\" Accessed May 15, 2021.  http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php"
    indent <- paste0(rep("&nbsp;", 8), collapse="")
    help <- paste0("Use sliders and pulldown menus to adjust conditions. Click the <b>Code</b> button to see code to reproduce the simulation. To learn more about the properties of a given float or wire, open an R console and type e.g. <br>", indent, "<tt>float(\"Kiel SFS40in\")</tt><br>or<br>", indent, "<tt>wire(\"1/4in wire/jack\")</tt><br>A list of float types is obtained with <br>", indent, "<tt>float(\"?\")</tt><br>and <br>", indent, "<tt>wire(\"?\")</tt><br>produces a list of wire types. See Deweey (1999, 2021) for more on these types.<br><b>References</b><br><ul><li>", dewey1999, "</li><li>", dewey2021, "</li></ul>")
    ui <- shiny::fluidPage(shiny::tags$style(shiny::HTML("body {font-family: 'Arial'; font-size: 10px; margin-left:1ex}")),
                           shiny::fluidRow(shiny::column(5, shiny::sliderInput("waterDepth",
                                                                               shiny::h6("Water Depth [m]"),
                                                                               min=10.0,  max=1000.0, value=200.0, step=1)),
                                           shiny::column(5, shiny::sliderInput("u",
                                                                               shiny::h6("Current [m/s]"),
                                                                               min=0.0, max=5.0, value=1.0, step=0.05)),

                                           shiny::column(2, shiny::selectInput("currentModel", "Current Model",
                                                                               choices=c("Constant",
                                                                                         "Linear",
                                                                                         "exp(z/30)",
                                                                                         "exp(z/100)",
                                                                                         "exp(z/300)"),
                                                                               selected="Constant"))),
                           shiny::fluidRow(shiny::column(5, shiny::uiOutput("wireLength")),
                                           shiny::column(2, shiny::radioButtons("wireOrder", "Order wire by", choices=c("name","buoyancy"),selected="name")),
                                           shiny::column(5, shiny::uiOutput("wireType"))),
                           shiny::fluidRow(shiny::column(2, shiny::radioButtons("floatOrder", "Order float by", choices=c("name","buoyancy"),selected="name")),
                                           shiny::column(5, shiny::uiOutput("floatType")),
                                           shiny::actionButton("help", "Help"),
                                           shiny::actionButton("code", "Code")),

                           shiny::fluidRow(shiny::plotOutput("plot")))


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
    server <- function(input, output, session)
    {
        shiny::observeEvent(input$help, {
                            shiny::showModal(shiny::modalDialog(shiny::HTML(help), title="Using this application", size="l"))
                           })

        shiny::observeEvent(input$code, {
                            waterDepth <- input$waterDepth
                            wireLength <- input$wireLength
                            u <- input$u
                            wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
                            floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
                            msg <- sprintf("%s<br>m <- mooring(anchor(depth=%g), wire(model=\"%s\", length=%g), float(model=\"%s\"))<br>", "library(mooring)", waterDepth, wireModel, wireLength, floatModel)
                            msg <- paste0(msg, "md <- discretise(m, by=1)<br>")
                            msg <- paste0(msg,
                                          "mdk <- knockdown(md, u=",
                                          switch (input$currentModel,
                                              "Constant"=sprintf("%g", input$u),
                                              "Linear"=sprintf("function(depth) %g*(1-depth/%g)", input$u, input$waterDepth),
                                              "exp(z/30)"=sprintf("function(depth) %g*exp(-depth/30)"),
                                              "exp(z/100)"=sprintf("function(depth) %g*exp(-depth/100)"),
                                              "exp(z/300)"=sprintf("function(depth) %g*exp(-depth/300)")),
                                          ")<br>")
                            msg <- paste0(msg, "par(mfrow=c(1, 2))<br>")
                            msg <- paste0(msg, "plot(mdk, which=\"tension\", fancy=TRUE, showDepths=FALSE)<br>")
                            msg <- paste0(msg, "plot(mdk, which=\"shape\", fancy=TRUE)<br>")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="R code", size="l"))
                           })

        output$wireLength <- shiny::renderUI({
            shiny::sliderInput("wireLength", "Wire Length [m]", min=1.0, max=input$waterDepth, value=input$waterDepth, step=1.0)
        })

        output$wireType <- shiny::renderUI({
            o <- if (input$wireOrder == "name") seq_along(wireChoices) else order(wireBuoyancy)
            shiny::selectInput("wireModel", "Wire Type",
                               choices=paste0(wireChoices[o], " [", wireBuoyancy[o], "kg/m]"),
                               selected="1/4in wire/jack [-0.13kg/m]")
        })

        output$floatType <- shiny::renderUI({
            o <- if (input$floatOrder == "name") seq_along(floatChoices) else order(floatBuoyancy)
            shiny::selectInput("floatModel", "Float Type",
                               choices=paste0(floatChoices[o], " [", floatBuoyancy[o], "kg]"),
                               selected="Kiel SFS40in [320kg]")
        })


        output$plot <- shiny::renderPlot({
            waterDepth <- input$waterDepth
            wireLength <- input$wireLength
            if (!is.null(wireLength)) {    # undefined at the start, since it depends on another slider
                u <- input$u
                wireModel <- gsub("[ ]+\\[.*kg/m\\]$", "", input$wireModel)
                floatModel <- gsub("[ ]+\\[.*kg\\]$", "", input$floatModel)
                #> message("currentModel='", input$currentModel, "'")
                #> message("waterDepth=", waterDepth)
                #> message("  wireLength=", wireLength)
                #> message("  u=", u)
                #> message("  wireModel=", wireModel)
                #> message("  floatModel=", floatModel)
                m <- mooring(anchor(depth=waterDepth), wire(model=wireModel, length=wireLength), float(model=floatModel))
                md <- discretise(m, 1)
                u <- switch(input$currentModel,
                            "Constant"=input$u,
                            "Linear"=function(depth) input$u*(1-depth/waterDepth),
                            "exp(z/30)"=function(depth) input$u*exp(-depth/30),
                            "exp(z/100)"=function(depth) input$u*exp(-depth/100),
                            "exp(z/300)"=function(depth) input$u*exp(-depth/300))
                mdk <- knockdown(md, u)
                par(mfrow=c(1,2))
                plot(mdk, which="tension", fancy=TRUE, showDepths=FALSE)
                plot(mdk, fancy=TRUE)
            }
        }, pointsize=12)#, height=500)
    }
    shiny::shinyApp(ui=ui, server=server)
}


