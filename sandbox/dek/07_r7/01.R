# install.packages("S7")
library(S7)
mooringElement <- new_class("mooringElement",
    properties = list(
        model = class_character,
        buoyancy = class_numeric,
        area = class_numeric,
        height = class_numeric,
        x = class_numeric,
        x0 = class_numeric,
        z = class_numeric,
        z0 = class_numeric,
        phi = class_numeric,
        tau = class_numeric
    )
)
float <- new_class("float", parent=mooringElement)
mooring <- new_class("mooring", properties = list(
    water_depth = class_numeric,
    elements = class_list
))

m1 <- mooring()
m1
m1@water_depth
m2 <- mooring(water_depth = 100)
m2
m2@water_depth
m1@elements[[1]] <- "hi"
m1@elements[[2]] <- 99.99
m1

flist <- list()
flist$dan <- float("dan", buoyancy=3.1)
flist$boy <- float("boy", buoyancy=1.3)
flist
names(flist)

