library(S7)
mooring <- new_class("mooring",
    package = "mooring",
    properties = list(
        elements = class_list, # holds mooringElement items
        waterDepth = class_numeric
    ),
    constructor = function(..., waterDepth=NA_real_) {
        #cat("in mooring constructor\n")
        elements <- list(...)
        #print(elements[[1]])
        if (!inherits(elements[[1]], "mooring::anchor")) stop("element 1 is not an anchor")
        if (is.na(waterDepth)) {
            waterDepth <- elements[[1]]@waterDepth
        } else {
            message("anchor depth takes precedence")
        }
        new_object(S7_object(), elements=elements, waterDepth=waterDepth)
    }
)

mooringElement <- new_class("mooringElement",
    package = "mooring",
    properties = list(
        model = class_character,
        buoyancy = class_numeric,
        height = class_numeric,
        area = class_numeric,
        CD = class_numeric,
        source = class_character,
        originalName = class_character
    )
)
anchor <- new_class("anchor",
    parent = mooringElement,
    package = "mooring",
    properties = list(waterDepth = class_numeric)
)
float <- new_class("float",
    parent = mooringElement,
    package = "mooring"
)
wire <- new_class("wire",
    parent = mooringElement,
    package = "mooring"
)
#f <- float()
#m <- mooring()
#f
#message("FIXME: add constructors")
