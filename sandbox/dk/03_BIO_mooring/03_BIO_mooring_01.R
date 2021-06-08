library(mooring)
m <- mooring(anchor(depth=1400),
             chain("5/8 galvanized chain", length=10),
             release("dual benthos 965-a")
             float("3 glass bub"),
             wire("3/16in jacketed", length=34),
             instrument("SBE37 microcat"),
             instrument("RCM11"),
             wire("3/16in jacketed", length=50),
             float("3 glass bub"),
             wire("3/16in jacketed", length=144)) # and more

