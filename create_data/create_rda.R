# Combine csv files to produce RDA file.

anchors <- NULL
for (file in list.files(pattern="^anchors.*csv$"))
    anchors <- rbind(anchors, read.csv(file))

chains <- NULL
for (file in list.files(pattern="^chains.*csv$"))
    chains <- rbind(chains, read.csv(file))

floats <- NULL
for (file in list.files(pattern="^floats.*csv$")) {
    message(file)
    floats <- rbind(floats, read.csv(file))
}

releases <- NULL
for (file in list.files(pattern="^releases.*csv$"))
    releases <- rbind(releases, read.csv(file))

wires <- NULL
for (file in list.files(pattern="^wires.*csv$"))
    wires <- rbind(wires, read.csv(file))

mooringElements <- list(anchors=anchors, floats=floats, chains=chains, releases=releases, wires=wires)
save(mooringElements, file="mooringElements.rda")

