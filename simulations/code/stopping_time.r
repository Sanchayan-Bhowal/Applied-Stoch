library(ggplot2)
library(plotly)

## interactive graph for 5 simulations of length 20-

N <- 20 ## length of walk
n <- 5 ## number of simulations

df <- data.frame(1:N)
df_index <- data.frame(1:N)
vec <- cumsum(sample(c(-1, 1), N - 1, replace = T))
df <- cbind(df_index, c(0, vec), rep("w 1", N))
colnames(df) <- c("time", "S", "nwalk")

for (i in 1:(n - 1)) {
  vec <- cumsum(sample(c(-1, 1), N - 1, replace = T))
  df_temp <- cbind(df_index, c(0, vec), rep(paste("w", as.character(i + 1)), N))
  colnames(df_temp) <- c("time", "S", "nwalk")
  df <- rbind(df, df_temp)
}
df$nwalk <- as.factor(df$nwalk)
g <- ggplot(df, aes(x = time, y = S, group = nwalk, color = nwalk)) +
  geom_line(show.legend = F)
ggplotly(g)

## larger number of simulations-

N <- 500 ## length of walk
n <- 100 ## number of simulations

df <- data.frame(1:N)
df_index <- data.frame(1:N)
vec <- cumsum(sample(c(-1, 1), N - 1, replace = T))
df <- cbind(df_index, c(0, vec), rep("w 1", N))
colnames(df) <- c("time", "S", "nwalk")


for (i in 1:(n - 1)) {
  vec <- cumsum(sample(c(-1, 1), N - 1, replace = T))
  df_temp <- cbind(df_index, c(0, vec), rep(paste("w", as.character(i + 1)), N))
  colnames(df_temp) <- c("time", "S", "nwalk")
  df <- rbind(df, df_temp)
}

df$nwalk <- as.factor(df$nwalk)
g <- ggplot(df, aes(x = time, y = S, group = nwalk, color = nwalk)) +
  geom_line(show.legend = F)
g <- g +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = 0, y = 0, xend = N, yend = N),
    show.legend = F, color = "black", linetype = 2
  ) +
  geom_segment(aes(x = 0, y = 0, xend = N, yend = -N),
    show.legend = F, color = "black", linetype = 2
  )
ggplotly(g)


## stopping time simulations-
n <- 500 ## number of simulations
N <- 20 ## length of walk
a <- 10 ## stopping time (T) is min of hitting time to a and N
s_t <- rep(0, n)
T <- rep(0, n)
for (i in 1:n) {
  s <- 0
  t <- 0
  while (s != a & t < N) {
    s <- s + sample(c(-1, 1), 1)
    t <- t + 1
  }
  s_t[i] <- s
  T[i] <- t
}

sample_mean <- cumsum(s_t) / 1:n

df <- data.frame(1:n, sample_mean, rep(0, n))
colnames(df) <- c("nsim", "sample_mean", "ref")
ggplot(df, aes(x = nsim, y = sample_mean)) +
  geom_line() +
  geom_line(aes(x = nsim, y = ref))

mean(s_t) ## should be close to zero
var(s_t)
mean(T) ## E[S_T^2]=E[T] theoretically, so var(s_t) should be close to mean(T)

df <- data.frame("S_T" = s_t)
g <- ggplot(df, aes(x = S_T)) +
  geom_histogram(binwidth = 2) +
  xlim(-20, 20) +
  geom_freqpoly(binwidth = 2) +
  labs(title = "Histogram for S_T")
ggplotly(g)