if (!file.exists("05_speed.rda")) {
    library(mooring)
    library(microbenchmark)
    w <- wire(length = 1)
    times <- 100
    t1 <- NULL
    t1sd <- NULL
    t2 <- NULL
    t2sd <- NULL
    Ns <- 10^seq(1, 4.5, 0.5)
    for (N in Ns) {
        a <- microbenchmark(
            {
                A <- list()
                for (i in seq_len(N)) {
                    A <- c(A, w)
                }
            },
            times = times
        )
        t1 <- c(t1, median(a$time) / 1e9) # seconds
        t1sd <- c(t1sd, sd(a$time) / 1e9) # seconds

        b <- microbenchmark(
            {
                A <- list()
                for (i in seq_len(N)) {
                    A[[i]] <- w
                }
            },
            times = times
        )
        t2 <- c(t2, median(b$time) / 1e9) # seconds
        t2sd <- c(t2sd, sd(b$time) / 1e9) # seconds
    }

    df <- data.frame(N = Ns, t1 = t1, t1sd = t1sd, t2 = t2, t2sd = t2sd)
    save(df, file = "05_speed.rda")
} else {
    load("05_speed.rda")
}
print(df)
png("05_speed.png", unit = "in", width = 7, height = 5, res = 150)
par(mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))
plot(df$N, df$t1,
    type = "o",
    log = "xy", pch = 20, ylim = range(c(df$t1, df$t2)),
    xlab = "Number of entries", ylab = "Time [s]"
)
points(df$N, df$t2, col = 2, pch = 20, type = "o")
grid()
eps <- diff(par("usr")[c(1, 2)]) / 75
for (j in seq_along(df$N)) {
    lines(rep(df$N[j], 2), df$t1[j] + df$t1sd[j] * c(-1, 1))
    lines(df$N[j] * c(1 - eps, 1 + eps), rep(df$t1[j] + df$t1sd[j], 2))
    lines(df$N[j] * c(1 - eps, 1 + eps), rep(df$t1[j] - df$t1sd[j], 2))
    lines(rep(df$N[j], 2), df$t2[j] + df$t2sd[j] * c(-1, 1), col = 2)
    lines(df$N[j] * c(1 - eps, 1 + eps), rep(df$t2[j] + df$t2sd[j], 2), col = 2)
    lines(df$N[j] * c(1 - eps, 1 + eps), rep(df$t2[j] - df$t2sd[j], 2), col = 2)
}
legend("topleft", pch = 20, lwd = 1, col = 1:2, legend = c("Combine using c()", "Combine using []"), bg = "white")
dev.off()
