# Read 'mdcodes.mat' file from Dewey's mooring package, and convert to local
# csv files. The column meanings are given in his moordesign.txt file, apart
# from an extra single-digit element at the end of each row, which here is
# called "code", because it is not mentioned in the moordesign.txt file.
#'
#' NOTE: all things reported in cm are converted to m here.
#
# Dewey, Richard K. “Mooring Design & Dynamics—a Matlab® Package for Designing
# and Analyzing Oceanographic Moorings.” Marine Models 1, no. 1 (December 1,
# 1999): 103–57.  https://doi.org/10.1016/S1369-9350(00)00002-X.


library(R.matlab)
d <- R.matlab::readMat("mdcodes.mat")
# Discover elements within the dataset
#> names(d)
# Set up names for columns based on guesses about Deweys' file.
names <- c("name","buoyancy","height","width","diameter","CD","code")

fixnames <- function(names)
{
    orig <- names
    names <- trimws(names)
    names <- gsub("\"", "in", names)
    names <- gsub("  ", " ", names)
    names <- gsub("([0-9]) in", "\\1in", names)
    names <- gsub("Billings-12in", "Billings 12in", names)
    names <- gsub("inViny", "in Viny", names)
    names <- gsub("^1/2 ", "1/2in ", names)
    names <- gsub("^1/4 ", "1/4in ", names)
    names <- gsub("^3/4 ", "3/4in ", names)
    names <- gsub("3/8 ", "3/8in ", names)
    names <- gsub("16 ", "16in ", names)
    names <- gsub("3/8shac", "3/8in shac", names)
    names <- gsub("^5/8 ", "5/8in ", names)
    names <- gsub("8' ", "8ft ", names)
    # Not sure whether the 1/2 in Dyneema is a width, so for caution change back.
    names <- gsub("1/2in Dyneema", "1/2 Dyneema", names)
    #print(data.frame(orig=orig, later=names))
    names
}

#> Use next to count spaces
#> count <- "      0.........1........2.........3.........4.........5.\n"
#> print(head(d$floats, 3))
# NOTE: was c(18,7,6,6,6,5,4) previously, but when I added 'anchors' I saw that was wrong
widths <- c(17, 8, 6, 6, 6, 5, 4)

# Anchors ('anchors') FIXME: maybe some of 'miscs' should go here, too.
anchors <- read.fwf(textConnection(d$anchors),widths=widths, col.names=names)
originalName <- anchors$name
anchors$name <- fixnames(anchors$name)
anchors$height <- anchors$height / 100
# NOTE: we do not use width, area or CD, because we
# are not solving for drag non these things
anchors$width <- NULL
anchors$diameter <- NULL
anchors$CD <- NULL
anchors <- cbind(anchors, source="Dewey")
anchors <- cbind(anchors, originalName=trimws(originalName))
write.csv(anchors, "anchors_dewey.csv", row.names=FALSE)


# Instruments ('cms') FIXME: maybe some of 'miscs' should go here, too.
#instruments <- read.fwf(textConnection(d$cms),widths=c(18,7,6,6,6,5,4), col.names=names)
instruments <- read.fwf(textConnection(d$cms),widths=widths, col.names=names)
originalName <- instruments$name
instruments$name <- fixnames(instruments$name)
instruments$height <- instruments$height / 100
instruments$area <- instruments$height * (instruments$width / 100)
instruments$width <- NULL              # have this info in area, so discard width
instruments$diameter <- NULL           # always 0, and we don't use it in this package
instruments <- cbind(instruments, source="Dewey")
instruments <- cbind(instruments, originalName=trimws(originalName))
for (w in which(0 == instruments$CD)) {
    instruments$CD[w] <- median(instruments$CD)
    warning("Float '", instruments$name[w], "': changed CD from 0 to ", instruments$CD[w], sep="")
}
write.csv(instruments, "instruments_dewey.csv", row.names=FALSE)


# Releases
#releases <- read.fwf(textConnection(d$acrel),widths=c(18,7,6,6,6,5,4), col.names=names)
releases <- read.fwf(textConnection(d$acrel),widths=widths, col.names=names)
originalName <- releases$name
releases$name <- fixnames(releases$name)
releases$height <- releases$height / 100
# note renaming, to keep area and height in adjacent columns
releases$width <- releases$height * releases$width / 100
names(releases) <- gsub("width", "area", names(releases))
# remove diameter, which is always 0 in Dewey's data for releases
releases$diameter <- NULL
releases <- cbind(releases, source="Dewey")
releases <- cbind(releases, originalName=trimws(originalName))
for (w in which(0 == releases$CD)) {
    releases$CD[w] <- median(releases$CD)
    warning("Float '", releases$name[w], "': changed CD from 0 to ", releases$CD[w], sep="")
}
write.csv(releases, "releases_dewey.csv", row.names=FALSE)


#floats <- read.fwf(textConnection(d$floats),widths=c(18,7,6,6,6,5,4), col.names=names)
floats <- read.fwf(textConnection(d$floats),widths=widths, col.names=names)
originalName <- floats$name
floats$name <- fixnames(floats$name)
floats$height <- floats$height / 100
# overwrite width (to keep it in same position after height
floats$width <- pi * (0.5 * floats$diameter / 100)^2
floats$diameter <- NULL
names(floats) <- gsub("width", "area", names(floats))
floats <- cbind(floats, source="Dewey")
floats <- cbind(floats, originalName=trimws(originalName))
for (w in which(0 == floats$CD)) {
    floats$CD[w] <- median(floats$CD)
    warning("Float '", floats$name[w], "': changed CD from 0 to ", floats$CD[w], sep="")
}
write.csv(floats, "floats_dewey.csv", row.names=FALSE)

# Wires. Dewey lists all diameters as zero, as a way to decode things
# later (I guess, to distinguish from floats) but we are not trying
# to make tidy data and there's no need for such tricks, so we call
# his 'width' as 'diameter'.
wires <- read.fwf(textConnection(d$wires),widths=widths, col.names=names)
originalName <- wires$name
wires$name <- fixnames(wires$name)
# Remove 'height', because it's meaningless for a wire.  (It's always 100 in the file.)
wires$height <- NULL
wires$areaPerMeter <- wires$width / 100
wires$width <- NULL # not being used
wires$diameter <- NULL # not being used
n <- names(wires)
n[n == "buoyancy"] <- "buoyancyPerMeter"
names(wires) <- n
wires <- cbind(wires, source="Dewey")
wires <- cbind(wires, originalName=trimws(originalName))
for (w in which(0 == wires$CD)) {
    wires$CD[w] <- median(wires$CD)
    warning("Wire '", wires$name[w], "': changed CD from 0 to ", wires$CD[w], sep="")
}
wires <- wires[, c("name", "buoyancyPerMeter", "areaPerMeter", "CD", "code", "source", "originalName")]
write.csv(wires, "wires_dewey.csv", row.names=FALSE)

# Chains and connectors
# I call things as 'chain' if they are used in variable lengths (and then I
# store buoyancyPerMeter, kg/m) and 'connector' otherwise (and then I store
# buoyancy, kg).
cc <- read.fwf(textConnection(d$chains),widths=widths, col.names=names)
originalName <- cc$name
cc$name <- fixnames(cc$name)
isChain <- cc$height == 100
chains <- cc[isChain, ]
N <- names(chains)
N[N=="buoyancy"] <- "buoyancyPerMeter"
N[N=="width"] <- "areaPerMeter"
names(chains) <- N
chains$height <- NULL
chains$areaPerMeter<- chains$areaPerMeter / 100 # convert to m
chains$name <- fixnames(chains$name)
chains <- cbind(chains, source="Dewey")
chains <- cbind(chains, originalName=trimws(originalName[isChain]))
for (w in which(0 == chains$CD)) {
    chains$CD[w] <- median(chains$CD)
    warning("Chain '", chains$name[w], "': changed CD from 0 to ", chains$CD[w], sep="")
}
chains$diameter <- NULL
write.csv(chains, "chains_dewey.csv", row.names=FALSE)

connectors <- cc[!isChain, ]
head(connectors,2)
connectors$diameter <- NULL # this is always 0, and we don't use it in this package
connectors$height <- connectors$height / 100
# replace width with area
connectors$width <- connectors$height * connectors$width / 100
names(connectors) <- gsub("width", "area", names(connectors))
connectors <- cbind(connectors, source="Dewey")
connectors <- cbind(connectors, originalName=trimws(originalName[!isChain]))
for (w in which(0 == connectors$CD)) {
    connectors$CD[w] <- median(connectors$CD)
    warning("Connector '", connectors$name[w], "': changed CD from 0 to ", connectors$CD[w], sep="")
}
write.csv(connectors, "connectors_dewey.csv", row.names=FALSE)

# FIXME: some of these are instruments, others floats, and still others
# ballasts. Should we move them to separate categories?

# We change # to ^, so it won't be a comment
miscs <- read.fwf(textConnection(gsub("#","^",d$miscs)), widths=widths, col.names=names)
miscs$name <- gsub("\\^", "#", miscs$name)
originalName <- miscs$name
miscs$name <- fixnames(miscs$name)
miscs$height <- miscs$height / 100
# we keep area (computed) and discard width and diameter
miscs$width <- miscs$width / 100
miscs$diameter <- miscs$diameter / 100
miscs$area <- ifelse(miscs$diameter == 0, miscs$height * miscs$width, pi*(miscs$diameter/2)^2)
miscs$width <- NULL
miscs$diameter <- NULL
miscs <- cbind(miscs, source="Dewey")
miscs <- cbind(miscs, originalName=trimws(originalName))
write.csv(miscs, "miscs_dewey.csv", row.names=FALSE)

