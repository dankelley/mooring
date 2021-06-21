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
d <- readMat("mdcodes.mat")
# Discover elements within the dataset
#> names(d)
# Set up names for columns.
names <- c("name","buoyancy","height","width","diameter","CD","code")

fixnames <- function(names)
{
    orig <- names
    names <- trimws(names)
    names <- gsub("\"", "in", names)
    names <- gsub("  ", " ", names)
    names <- gsub("^1 ", "1in ", names)
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
floats <- read.fwf(textConnection(d$floats),widths=c(18,7,6,6,6,5,4), col.names=names)
originalName <- floats$name
floats$name <- fixnames(floats$name)
floats$height <- floats$height / 100
floats$width <- NULL # this is always 0, and we don't use it in this package
floats$diameter <- floats$diameter / 100
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
wires <- read.fwf(textConnection(d$wires),widths=c(18,7,6,6,6,5,4), col.names=names)
originalName <- wires$name
wires$name <- fixnames(wires$name)
# Remove 'height', because it's meaningless for a wire.  (It's always 100 in the file.)
wires$height <- NULL
wires$diameter <- wires$width / 100 # we will call it diameter, which makes more sense
wires$width <- NULL
n <- names(wires)
n[n == "buoyancy"] <- "buoyancyPerMeter"
names(wires) <- n
wires <- cbind(wires, source="Dewey")
wires <- cbind(wires, originalName=trimws(originalName))
for (w in which(0 == wires$CD)) {
    wires$CD[w] <- median(wires$CD)
    warning("Wire '", wires$name[w], "': changed CD from 0 to ", wires$CD[w], sep="")
}
write.csv(wires, "wires_dewey.csv", row.names=FALSE)

# Chains and connectors
# I call things as 'chain' if they are used in variable lengths (and then I
# store buoyancyPerMeter, kg/m) and 'connector' otherwise (and then I store
# buoyancy, kg).
cc <- read.fwf(textConnection(d$chains),widths=c(18,7,6,6,6,5,4), col.names=names)
originalName <- cc$name
cc$name <- fixnames(cc$name)
isChain <- cc$height == 100
chains <- cc[isChain, ]
N <- names(chains)
N[N=="buoyancy"] <- "buoyancyPerMeter"
names(chains) <- N
chains$height <- NULL
chains$width <- chains$width / 100 # convert to m
chains$diameter <- NULL # is 0 anyway
chains$name <- fixnames(chains$name)
chains <- cbind(chains, source="Dewey")
chains <- cbind(chains, originalName=trimws(originalName[isChain]))
for (w in which(0 == chains$CD)) {
    chains$CD[w] <- median(chains$CD)
    warning("Chain '", chains$name[w], "': changed CD from 0 to ", chains$CD[w], sep="")
}
write.csv(chains, "chains_dewey.csv", row.names=FALSE)

connectors <- cc[!isChain, ]
connectors
connectors$diameter <- NULL # this is always 0, and we don't use it in this package
connectors$height <- connectors$height / 100
connectors$width <- connectors$width / 100
connectors <- cbind(connectors, source="Dewey")
connectors <- cbind(connectors, originalName=trimws(originalName[!isChain]))
for (w in which(0 == connectors$CD)) {
    connectors$CD[w] <- median(connectors$CD)
    warning("Connector '", connectors$name[w], "': changed CD from 0 to ", connectors$CD[w], sep="")
}
write.csv(connectors, "connectors_dewey.csv", row.names=FALSE)


