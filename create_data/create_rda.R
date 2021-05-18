# Combine csv files to produce RDA file.

wires <- NULL
for (file in list.files(pattern="^wires.*csv$"))
    wires <- rbind(wires, read.csv(file))
chains <- NULL
for (file in list.files(pattern="^chains.*csv$"))
    chains <- rbind(chains, read.csv(file))
floats <- NULL
for (file in list.files(pattern="^floats.*csv$"))
    floats <- rbind(floats, read.csv(file))
mooringElements <- list(wires=wires, chains=chains, floats=floats)
save(mooringElements, file="mooringElements.rda")

