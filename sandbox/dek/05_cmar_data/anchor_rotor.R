# Next 3 lines use data from a CMARS email dated 2023-12-14
weight <- 8 # Mass: 8 kg
R <- 0.146 # Radius: 0.146 m
H <- 0.0635 # #Height (from the floor to the top of the rotor in rotor_xz.png): 0.0635 m

library(mooring)
n <- 10
b <- buoyancyCalculation(8) # -4.577
df <- data.frame(name = paste(1:n, "rotor"),
    buoyancy = round(1:n * b, 3),
    height = H,
    area = round(2 * R * H, 5),
    CD = 1.4, code=NA, source="CMAR", originalName="-")
write.csv(df, "anchors_cmar.csv")
