library(mooring) # for buoyancyCalculation()

# 1. Setup: ingest CMAR-supplied csv file
message("Step 1: ingest CMAR csv file ...")
gs <- read.csv("cmar_gear_specs.csv")
ifloats <- grep("float", gs$name)
iwires <- grep("polypropylene", gs$name)
iinstruments <- -c(ifloats, iwires)
floats <- gs[ifloats, ]
wires <- gs[iwires, ]
instruments <- gs[iinstruments, ]
message("    ... done.")

# 2. ANCHORS (based on an email from DD dted 2023-12-24)
# Next 3 lines use data from a CMARS email dated 2023-12-14
message("Step 2: create anchors_cmar.csv file ...")
weight <- 8 # Mass: 8 kg
R <- 0.146 # Radius: 0.146 m
H <- 0.0635 # Height (from the floor to the top of the rotor in rotor_xz.png): 0.0635 m
n <- 10 # make this many items (although I think CMAR limits to 7)
b <- buoyancyCalculation(8, rho = 7850) # -6.95 kg
anchorsDF <- data.frame(
    name = paste(1:n, "rotor"),
    buoyancy = round(1:n * b, 3),
    height = H + (1:n-1) * 0.6*0.0254,
    area = round(2 * R * 1:n * H, 5),
    CD = 1.3, code = NA, source = "CMAR", originalName = "-"
)
write.csv(anchorsDF, "anchors_cmar.csv", row.names = FALSE)
message("    ... done.")

# 3. FLOATS
# Required format (from ../../floats_bio.csv)
#    name,buoyancy,height,area,CD,code,source,originalName
#    new glass streamlined float c2,        45.41, 0.87,  0.183, 0.6,NA,BIO,"NEW GLASS STREAMLINED FLOAT (C2)"
message("Step 3: create floats_cmar.csv file ...")
floatsDF <- data.frame(
    name = floats$name,
    buoyancy = round(as.numeric(floats$buoyancySalt), 3),
    height = round(as.numeric(floats$height), 3),
    area = round(as.numeric(floats$area), 6),
    CD = 0.6, # a guess
    code = NA,
    source = "CMAR",
    originalName = floats$name
)
write.csv(floatsDF, file = "floats_cmar.csv", row.names = FALSE)
message("    ... done")


# 4. WIRES
message("Step 4: create wires_cmar.csv file ...")
# Below is what a 'wire' CSV file should contain (first 2 lines of Dewey dataset)
#     "name","buoyancyPerMeter","areaPerMeter","CD","code","source","originalName"
#     "1/4in Kevlar",-0.009,0.007,1.3,6,"Dewey","1/4 Kevlar"
# Below is what the CMAR CSV file contains
#     wires$weightAir_kg # "28.1227kg/1200 ft"
#     specific_gravity: chr "1.30 (sinks)"
# Note: by contrast, Dewey's polyprop floats
#     wire("3/4in Polyprop", length = 1)$buoyancy # 0.016
wpm <- 28.1227 / (1200 / 3.28084) # weight/metre; next is for buoyancy/metre
bpm <- round(buoyancyCalculation(weight = wpm, rho = 1.30 * 1027), 4) # -0.0439
wiresDF <- data.frame(
    name = "3/8in leaded polypropylene",
    buoyancyPerMeter = bpm,
    areaPerMeter = wires$areaPerMeter,
    CD = 1.3,
    code = NA,
    source = "CMAR",
    originalName = "3/8in leaded polypropylene"
)
write.csv(wiresDF, file = "wires_cmar.csv", row.names = FALSE)
message("    ... done")

# 5. INSTRUMENTS
message("Step 5 create instruments_cmar.csv file ...")
# Below (from ../../instruments_bio.csv) is the required format
#    name,buoyancy,height,area,CD,code,source,originalName
#    "seacat 16-03 (bar,plastic case)",-7.49, 0.84,0.122,0.65,NA,BIO,"SEACAT 16-03 (BAR, PLASTIC CASE)"
instrumentsDF <- data.frame(
    name = instruments$name,
    buoyancy = round(as.numeric(instruments$buoyancySalt), 4),
    height = round(as.numeric(instruments$height), 3),
    area = round(as.numeric(instruments$area), 6),
    CD = 1.3, code = NA,
    source = "CMAR", originalName = instruments$name
)
write.csv(instrumentsDF, file = "instruments_cmar.csv", row.names = FALSE)
message("    ... done")
