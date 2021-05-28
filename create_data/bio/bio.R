rm(list=ls())
lines <- readLines("moorspec-4.csv")
lines2 <- gsub("  ", " ", lines)
lines3 <- gsub("\\*\\*", "", lines2)
# change some odd special-case things
lines4 <- gsub("\\(-1500\\)", "-1500", lines3)
lines5 <- gsub("nt/m", "", lines4)
lines6 <- gsub("\\(15\\)*", "15", lines5)
lines7 <- gsub("15\\*", "15", lines6)

d <- read.csv(text=lines7)
names(d)
# chop out some things, just to get it so I can read the key columns on my screen
d <- d[, -grep("number", names(d))]    # first column
d <- d[, -grep("^X", names(d))]        # empty columns at end
d <- d[, -grep("length1", names(d))]   # not sure what the two length columns are
d <- d[, -grep("LBS", names(d))]       # we have Newtons beside it
# We will ignore some other columns, just by not using them.
print(names(d))
head(d,3)
type <- tolower(d$description)
height <- as.numeric(d$length2)
area <- as.numeric(d$area)
weightPerMetre <- as.numeric(d$weight_per_m)
d$NEWTONS
newtons <- as.numeric(d$NEWTONS)
buoyancy <- newtons / 9.8
dd <- data.frame(type=type, weightPerMetre, buoyancy=buoyancy, height=height, area=area)

# CHECK: line 1: this gives buoyancy 0.07755102 kg/m, while Dewey gives 0.007. So,
# something is wrong.  The line in the BIO file says "0.76nt/m" so I divided that by
# 9.8 to get what I call buoyancy here (for a wire type, it's buoyancy/metre).
dd[1,]
print(head(dd))
