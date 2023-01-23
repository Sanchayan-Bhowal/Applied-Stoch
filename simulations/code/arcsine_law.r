# Simulation: Arc-sine law
# Written by: Ankan Kar(bmat2001), Rahil Miraj(bmat2013)

########
# Problem:
# Let S_n=\sum_{i=1}^{n}X_i, where X_i belongs to {-1,1} for i=1,2,...,2N and
# let L=max{0<=n<=2N: S_n=0}.
# Then P(L/2N<=x)~(2/pi)arcsin(\sqrt(pi)) as N goes to infinity.
########


N <- 1000 # Length of the walk
w <- 10000 # Number of times we perform the walk for the simulation.
L <- c() # We take empty array where we will
# store the values of L for each time we perform the walk.

for (j in 1:w) {
  a1 <- sample(c(-1, 1), size = 2 * N, replace = TRUE) # Walk of length 2N.
  a <- c(0, a1)
  s <- cumsum(a) # S_n
  s
  mx <- 0
  i <- 2 * N + 1
  while (i >= 1) {
    if (s[i] == 0) {
      mx <- i - 1
      break
    }
    i <- i - 1
  }
  L[j] <- mx # Appending the values in the array for L.
}
# Empty array to store the values of arcsine and the
# required probability, which we will plot.
arcsine <- c()
prob <- c()

# Taking x between [0,1] with increament 0.01 and each
# time we get (L/2N)<=x, we count 1 and at the end we
# divide the total number of times we have counted 1 by w,
# the length of L,which gives the required probability.
x <- 0
i <- 1
while (x <= 1) {
  count <- 0
  for (j in 1:w) {
    if (L[j] / (2 * N) <= x) {
      count <- count + 1
    }
  }
  prob[i] <- count / w
  arcsine[i] <- 2 * asin(sqrt(x)) / pi
  i <- i + 1
  x <- x + 0.01
}
# Plots
windows()
plot(
  x = 1:100, y = prob, xaxt = "n",
  type = "scatter", xlab = "x", ylab = "Probability"
)
par(new = TRUE)
t <- seq(from = 0, to = 1, by = 0.01)
plot(t, 2 * asin(sqrt(t)) / pi,
  col = "red",
  ylab = "", xlab = "", yaxt = "n", type = "l"
)
legend(0.01, 0.95,
  legend = c("P(L/2N < x)", "(2/pi)arcsin(sqrt(x))"),
  lty = 1:1, col = c("black", "red"), cex = 0.9
)