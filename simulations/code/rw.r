n <- 10000
N <- 5000
s <- c()
for (i in 1:n) {
    s <- c(s, sum(sample(c(-1, 1), size = N, replace = T)))
}

# X-axis grid
x2 <- seq(-N, N, length = 1000)

# Normal curve
fun <- dnorm(x2, mean = 0, sd = sqrt(N))

windows()
# Histogram
hist(s,
    prob = TRUE,
    ylim = c(0, max(fun)),
    main = "Random Walk distribution with normal curve"
)
lines(x2, fun, col = 2, lwd = 2)