library(S7)


# install.packages("S7")
oce <- new_class("oce",
    properties = list(
        metadata = class_list,
        data = class_list,
        processingLog = class_list
    )
)

ctd <- new_class("ctd",
    parent = oce,
    constructor = function(salinity, temperature, pressure) {
        time <- oce::presentTime()
        message <- "create ctd object"
        new_object(oce(),
            data = list(salinity = salinity, temperature = temperature, pressure = pressure),
            processingLog = list(time = time, message = message)
        )
    }
)

# Access
`[[` <- new_generic("[[", "x", function(x, ...) {
    S7_dispatch()
})
method(`[[`, oce) <- function(x, name, ...) {
    # unname(x@data[name]) # not OK
    # unlist(x@data[name]) # not OK
    unlist(unname(x@data[name])) # OK
    # unlist(as.vector(unname(x@data[name]))) # OK
}

# summary
summary <- new_generic("summary", "x")
method(summary, oce) <- function(x) {
    cat("data contains: \"", paste(names(x@data), collapse = "\", \""), "\"\n", sep = "")
    cat("metadata contains: \"", paste(names(x@metadata), collapse = "\", \""), "\"\n", sep = "")
}


# Create and test a ctd object
a <- ctd(c(30, 31), c(15, 14), c(0, 10))
summary(a)
print(a@data$salinity)
print(a@data$temperature)
print(a@data$pressure)
print(a@processingLog)
print(a[["salinity"]])

