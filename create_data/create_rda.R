# Combine csv files to produce RDA file.

debug <- 0

anchors <- NULL
for (file in list.files(pattern="^anchors.*csv$")) {
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
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
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
    if (debug > 0)
        print(d)
    if (any(d$name %in% chains$name))
        stop("name conflict")
    for (item in c("buoyancyPerMeter", "areaPerMeter", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    chains <- rbind(chains, d)
}

connectors <- NULL
for (file in list.files(pattern="^connectors.*csv$")) {
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
    if (debug > 0)
        print(d)
    if (any(d$name %in% connectors$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "area", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    connectors <- rbind(connectors, d)
}

floats <- NULL
for (file in list.files(pattern="^floats.*csv$")) {
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
    if (debug > 0)
        print(d)
    if (any(d$name %in% floats$name))
        stop("name conflict")
    for (item in c("buoyancy", "height", "area", "CD")) {
        if (class(d[[item]]) != "numeric" && class(d[[item]]) != "integer")
            stop("'", item, "' is not numeric or integer; it is ", class(d[[item]]))
        d[[item]] <- as.numeric(d[[item]])
    }
    floats <- rbind(floats, d)
}

instruments <- NULL
for (file in list.files(pattern="^instruments.*csv$")) {
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
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
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
    d$area <- round(d$area, 4) # seems the limit of what could be measured on height etc
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
# reorder columns for consistency
miscsOrig <- miscs
miscs <- miscsOrig[, c("name", "buoyancy", "height", "area", "CD", "code", "source", "originalName")]

releases <- NULL
for (file in list.files(pattern="^releases.*csv$")) {
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
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
    d <- read.csv(file)
    message(paste(file, "has", length(names(d)), "cols"))
    if (debug > 0)
        print(head(d,2))
    if (any(d$name %in% wires$name))
        stop("name conflict")
    for (item in c("buoyancyPerMeter", "areaPerMeter", "CD")) {
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

