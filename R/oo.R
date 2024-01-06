#' @importFrom S7 new_class new_object S7_object
mooringS7 <- S7::new_class("mooringS7",
    package = "mooring",
    properties = list(
        elements = class_list, # holds mooringElement items
        waterDepth = class_numeric,
        u = class_any
    ),
    constructor = function(..., waterDepth = NA_real_) {
        #cat("in mooring constructor\n")
        elements <- unlist(list(...)[[1]]) # FIXME: why is this so complicated?
        #cat("elements[[1]] follows\n");print(elements[[1]])
        if (!is.anchor(elements[[1]])) stop("element 1 is not an anchor")
        if (is.na(waterDepth)) stop("must specify waterDepth")
        new_object(S7_object(), elements = elements, waterDepth = waterDepth, u = 0.0)
    }
)

mooringElementS7 <- S7::new_class("mooringElementS7",
    package = "mooring",
    properties = list(
        model = class_character,
        buoyancy = class_numeric,
        height = class_numeric,
        area = class_numeric,
        CD = class_numeric,
        source = new_property(class_character, default = ""),
        originalName = new_property(class_character, default = ""),
        x = new_property(class_numeric, default = 0.0),
        x0 = new_property(class_numeric, default = 0.0),
        z = new_property(class_numeric, default = 0.0),
        z0 = new_property(class_numeric, default = 0.0),
        phi = new_property(class_numeric, default = 0.0),
        tau = new_property(class_numeric, default = 0.0),
        group = new_property(class_numeric, default = 0)
    ) # ,
    # constructor = function(...) {
    #    # cat("in mooringElement constructor\n")
    #    elements <- list(...)
    #    print(names(elements))
    #    imodel <- grep("^mod", names(elements))
    #    ioriginalName <- grep("^orig", names(elements))
    #    if (!length(ioriginalName) && length(imodel) > 0L) {
    #        cat("BEFORE:\n");print(elements)
    #        elements <- c(elements, originalName = elements[[imodel[1]]])
    #        cat("AFTER:\n");print(elements)
    #    }
    #    new_object(S7_object(), elements = elements)
    # }
)
# properties = list(waterDepth = class_numeric)
anchorS7 <- S7::new_class("anchorS7", parent = mooringElementS7, package = "mooring")
chainS7 <- S7::new_class("chainS7", parent = mooringElementS7, package = "mooring")
connectorS7 <- S7::new_class("connectorS7", parent = mooringElementS7, package = "mooring")
floatS7 <- S7::new_class("floatS7", parent = mooringElementS7, package = "mooring")
instrumentS7 <- S7::new_class("instrumentS7", parent = mooringElementS7, package = "mooring")
miscS7 <- S7::new_class("miscS7", parent = mooringElementS7, package = "mooring")
releaseS7 <- S7::new_class("releaseS7", parent = mooringElementS7, package = "mooring")
wireS7 <- S7::new_class("wireS7", parent = mooringElementS7, package = "mooring")
