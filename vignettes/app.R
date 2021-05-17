# vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

references <- list(FinkeSiedler1986="Finke, M., and G. Siedler, 1986. “Drag Coefficients of Oceanographic Mooring Components.” Journal of Atmospheric and Oceanic Technology 3(2):255-264")


author <- "Dan Kelley"
affiliation <- "Dalhousie University Physical Oceanography"
title <- "Simple Mooring"
version <- "0.1"
time <- "2021-05-15"
copyright <- paste0("\U00A9 ", substr(time, 1, 4), " ", author)
url <- "https://services.dal.ace-net.ca/shiny/dkelley/2021/simple_mooring/"

colBottom <- "tan"
colStagnant <- "gray"
g <- 9.8                               # acceleration due to gravity [m/s^2]
rho0 <- 1027                           # fluid density [kg/m^3]
thetaConvergenceCriterion <- 0.05 * pi / 180 # will report theta to 0.1deg
thetaIterationsMax <- 100

# Line diameter and CD
# https://deepwaterbuoyancy.com/wp-content/uploads/mooring-line-fairings-deepwater-buoyancy.pdf
# says drag is 1.2 for 3/8" wire cable.
lineCD <- 1.2

debug <- TRUE                          # for developer to debug the code
dmsg <- function(...)
    if (debug) cat(file=stderr(), ..., "\n")

library(shiny)

floatData <- read.table("float_data.dat", header=TRUE)
lineData <- read.table("line_data.dat", header=TRUE)

ui <- fluidPage(headerPanel(title="", windowTitle=title),
                tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
                style="text-indent:1em; background:#e6f3ff",
                fluidRow(column(3, p(paste0(title, " [", version, "]")), style="font-weight:bold; color:blue;"),
                         column(3, copyright, style="color:blue;"),
                         column(1, actionButton("help", "Help")),
                         column(2, actionButton("citation", "Citation"))),
                fluidRow(column(4, selectInput(inputId="floatType", label="Float Type",
                                               choices=floatData$name, selected="SSF-30")),
                         column(4, selectInput(inputId="lineType", label="Line Type",
                                               choices=lineData$name, selected="3/8-inch wire"))),
                fluidRow(column(4, sliderInput(inputId="l", label="Line length [m]",
                                               value=100.0, min=10.0, max=1000.0)),
                         column(4, sliderInput(inputId="u", label=HTML("u [m/s]"),
                                               value=0.5, min=0.0, max=2.0, step=0.05))),
                fluidRow(plotOutput("plot")))

server <- function(input, output, session)
{

    observeEvent(input$help,
                 {
                     msg <- HTML(paste(readLines("README.html"), collapse="\n"))
                     showModal(modalDialog(msg, title="Data Sources", size="l", easyClose=TRUE))
                }
    )                                  # help

    observeEvent(input$citation,
                 {
                     msg <- paste0(author, ". ", title, " [version ", version, "], ", affiliation, ", ", time, ". ", url, ".")
                     showModal(modalDialog(HTML(msg), title="Citing this application.", size="l"))
                 }
    )                                  # citation

    output$plot <- renderPlot({
        u <- input$u # FIXME: let be depth-variable
        floatType <- input$floatType
        lineType <- input$lineType
        l <- input$l
        if (!(floatType %in% floatData$name)) {
            floatType <- floatData$name[1]
            dmsg("ERROR: unknown Float Type, so using ", floatType, " as a default.")
        }
        w <- which(floatType == floatData$name)
        buoyancy <- floatData$buoyancy[w]
        floatCD <- floatData$CD[w]
        # FIXME: assuming spherical float
        floatA <- pi * floatData$radius_m[w]^2
        dmsg(sprintf("Float type %s: projected area %.2g m^2; buoyancy %.0f kg",
                     floatType, floatA, buoyancy))
        if (!(input$lineType %in% lineData$name)) {
            lineType <- lineData$name[1]
            dmsg("ERROR: unknown Line Type, so using ", lineType, " as a default.")
        }
        w <- which(lineType == lineData$name)
        lineRadius <- lineData$radius_m[w]
        lineCD <- lineData$CD[w]
        # B, buoyancy force, is in the vertical
        B <- g * buoyancy
        # D, drag force, is in the horizontal
        u2 <- u * abs(u)
        theta <- 0 # start of iterative procedure
        thetaLast <- theta
        for (i in 1:100) {
            iterations <- i
            lineA <- 2 * l * lineRadius * cos(theta)
            #> dmsg("iteration ", i, " lineA=", lineA, " lineCD=", lineCD, " floatA=", floatA, " floatCD=", floatCD, sep="")
            D <- 0.5 * rho0 * u2 * (floatA*floatCD + lineA*lineCD)
            theta <- atan2(D, B)
            # dmsg(sprintf("  Iteration %d: theta=%.3f deg", i, 180/pi*theta))
            # dmsg("theta=", theta, ", thetaLast=", thetaLast, "delta theta=", theta-thetaLast)
            if (abs(theta - thetaLast) < thetaConvergenceCriterion)
                break
            thetaLast <- theta
        }
        x <- l * sin(theta)
        z <- l * cos(theta)
        dmsg(sprintf("B=%0.0fN, D=%.0fN, theta=%.2gdeg after %d iterations\n",
                     B, D, 180/pi*theta, iterations))
        if (iterations > thetaIterationsMax)
            cat(file=stderr(), "warning: more than ", thetaIterationsMax, "iterations")

        par(mar=c(0,0,1,0))
        ylim <- c(-0.04*l, 1.04*l)
        plot(c(-x, x), c(0, z), ylim=ylim, type="n", asp=1, xlab="", ylab="", axes=FALSE)
        lines(c(0, 0), c(0, l), col=colBottom)
        lines(c(0, x), c(0, z))
        box()
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], 0, col=colBottom)
        points(x, z, pch=20)
        points(0, l, pch=20, col=colStagnant)
        tensionNewton <- B / cos(theta)
        tensionStress <- tensionNewton / (pi * lineRadius^2)
        tensionKg <- tensionNewton / g
        #mtext(sprintf("Knockdown %.1f m for %.1f m line. Line tension %.0fN (%.1f kg). Line Stress %.0fMPa.",
        #              l-z, l, tensionNewton, tensionKg, tensionStress/1e6))
        mtext(sprintf("Knockdown %.1f m for %.1f m line. Line tension %.1f kg", l-z, l, tensionKg))
    }, height="auto", pointsize=18)                     # plot

    observeEvent(input$keypressTrigger, {
                 key <- intToUtf8(input$keypress)
                 dmsg("key='",key, "'\n", sep="")
                 #if (key == "d") {
                 #    debug <- !debug
                 #} else if (key == "?") {
                 if (key == "?") {
                     showModal(modalDialog(title="Key-stroke commands",
                                           HTML("<ul>
                                                <li> <b>d</b>: toggle debugging (output to console)</li>
                                                <li> <b>?</b>: display this message</li>
                                                </ul>"), easyClose=TRUE))
                 }
    })                                 # keypressTrigger

}                                      # server

shinyApp(ui=ui, server=server)

