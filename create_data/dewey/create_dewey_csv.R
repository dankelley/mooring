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

#> Use next to count spaces
#> count <- "      0.........1........2.........3.........4.........5.\n"
#> print(head(d$floats, 3))
#> cat(count, sep="")
floats <- read.fwf(textConnection(d$floats),widths=c(18,7,6,6,6,5,4), col.names=names)
floats$name <- trimws(floats$name)
floats$height <- floats$height / 100
floats$width <- NULL # this is always 0, and we don't use it in this package
floats$diameter <- floats$diameter / 100
floats <- cbind(floats, source="Dewey")
write.csv(floats, "floats_dewey.csv", row.names=FALSE)

# Wires. Dewey lists all diameters as zero, as a way to decode things
# later (I guess, to distinguish from floats) but we are not trying
# to make tidy data and there's no need for such tricks, so we call
# his 'width' as 'diameter'.
wires <- read.fwf(textConnection(d$wires),widths=c(18,7,6,6,6,5,4), col.names=names)
wires$name <- trimws(wires$name)
# Remove 'height', because it's meaningless for a wire.  (It's always 100 in the file.)
wires$height <- NULL
wires$diameter <- wires$width / 100 # we will call it diameter, which makes more sense
wires$width <- NULL
n <- names(wires)
n[n == "buoyancy"] <- "buoyancyPerMeter"
names(wires) <- n
wires <- cbind(wires, source="Dewey")
write.csv(wires, "wires_dewey.csv", row.names=FALSE)

# Chains
chains <- read.fwf(textConnection(d$chains),widths=c(18,7,6,6,6,5,4), col.names=names)
chains$name <- trimws(chains$name)
chains$height <- chains$height / 100
chains$width <- chains$width / 100
chains$diameter <- NULL # this is always 0, and we don't use it in this package
chains <- cbind(chains, source="Dewey")
write.csv(chains, "chains_dewey.csv", row.names=FALSE)


