# Written by: Sanchayan Bhowal(bmat2023)

n <- 10000 # no. of simulations
N <- 5000 # length of the walk
s <- c() # vector to contain S_k for n simulations

# performing n simulations
for (i in 1:n) {
    s <- c(s, sum( # sum of X_i
        sample(c(-1, 1), size = N, replace = T) # generating X_i
    ))
}

# X-axis grid
x2 <- seq(-N, N, length = 1000)

# Normal curve
func <- function(x) dnorm(x, mean = 0, sd = sqrt(N))

df <- data.frame(rand = s)

library(ggplot2)
library(plotly)
library(hrbrthemes)

p <- ggplotly(ggplot(df, aes(x = rand)) + # input data in ggplot2
    geom_histogram(
        mapping = aes(y = ..density..), # plotting normalised histogram
        binwidth = 15, # setting class size
        fill = "#69b3a2", color = "#e9ecef", alpha = 0.9
    ) +
    xlab("") +
    ylab("") +
    labs(title = "Random Walk distribution with normal curve") +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)))
v <- seq(min(s), max(s), 1)
p <- p %>% add_lines(
    x = v, y = func(v),
    line = list(color = "#B3697A")
) # creates the curve of the normal distribution

htmlwidgets::saveWidget(
    widget = p, # the plotly object
    file = "normal_approx.html", # the path & file name
    selfcontained = TRUE # creates a single html file
)

print(p)