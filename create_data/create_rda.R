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
    for (item in c("buoyancyPerMeter", "width", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
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
    for (item in c("buoyancy", "height", "width", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
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
    for (item in c("buoyancy", "height", "diameter", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
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

miscs <- NULL
for (file in list.files(pattern="^miscs.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% miscs$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "area", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    miscs <- rbind(miscs, d)
}

releases <- NULL
for (file in list.files(pattern="^releases.*csv$")) {
    message(file)
    d <- read.csv(file)
    if (debug > 0)
        print(d)
    if (any(d$name %in% releases$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "area", "CD")) {
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
    for (item in c("buoyancyPerMeter", "diameter", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    wires <- rbind(wires, d)
}

mooringElements <- list(anchors=anchors,
                        chains=chains,
                        connectors=connectors,
                        floats=floats,
                        instruments=instruments,
                        miscs=miscs,
                        releases=releases,
                        wires=wires)
save(mooringElements, file="mooringElements.rda")

