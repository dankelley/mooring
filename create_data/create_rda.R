# Combine csv files to produce RDA file.

debug <- 0

anchors <- NULL
for (file in list.files(pattern="^anchors.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% anchors$name))
        stop("name conflict")
    for (item in c("buoyancy", "height")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    anchors <- rbind(anchors, d)
}

chains <- NULL
for (file in list.files(pattern="^chains.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% chains$name))
        stop("name conflict")
    if (class(d$buoyancyPerMeter) != "numeric") stop("buoyancyPerMeter is not numeric; it is ", class(d$buoyancyPerMeter))
    if (class(d$width) != "numeric") stop("width is not numeric; it is ", class(d$width))
    if (class(d$CD) != "numeric") stop("CD is not numeric; it is ", class(d$CD))
    chains <- rbind(chains, d)
}

connectors <- NULL
for (file in list.files(pattern="^connectors.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% connectors$name))
        stop("name conflict")
    if (class(d$buoyancy) != "numeric") stop("buoyancy is not numeric; it is ", class(d$buoyancy))
    if (class(d$height) != "numeric") stop("height is not numeric; it is ", class(d$length))
    if (class(d$width) != "numeric") stop("width is not numeric; it is ", class(d$width))
    if (class(d$CD) != "numeric") stop("CD is not numeric; it is ", class(d$CD))
    connectors <- rbind(connectors, d)
}

floats <- NULL
for (file in list.files(pattern="^floats.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% floats$name))
        stop("name conflict")
    if (class(d$buoyancy) != "numeric") stop("buoyancy is not numeric; it is ", class(d$buoyancy))
    if (class(d$height) != "numeric") stop("height is not numeric; it is ", class(d$height))
    if (class(d$diameter) != "numeric") stop("diameter is not numeric; it is ", class(d$diameter))
    if (class(d$CD) != "numeric") stop("CD is not numeric; it is ", class(d$CD))
    floats <- rbind(floats, d)
}

instruments <- NULL
for (file in list.files(pattern="^instruments.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% instruments$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "area", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    instruments <- rbind(instruments, d)
}

releases <- NULL
for (file in list.files(pattern="^releases.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% releases$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "width", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    releases <- rbind(releases, d)
}

wires <- NULL
for (file in list.files(pattern="^wires.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% wires$name))
        stop("name conflict")
    if (class(d$buoyancyPerMeter) != "numeric") stop("buoyancy is not numeric; it is ", class(d$buoyancyPerMeter))
    if (class(d$diameter) != "numeric") stop("diameter is not numeric; it is ", class(d$diameter))
    if (class(d$CD) != "numeric") stop("CD is not numeric; it is ", class(d$CD))
    wires <- rbind(wires, d)
}

mooringElements <- list(anchors=anchors,
                        chains=chains,
                        connectors=connectors,
                        floats=floats,
                        instruments=instruments,
                        releases=releases,
                        wires=wires)
save(mooringElements, file="mooringElements.rda")

