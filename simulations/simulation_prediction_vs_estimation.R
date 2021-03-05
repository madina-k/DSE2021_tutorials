pacman::p_load(tidyverse)
set.seed(1234)

# Parameters
nsim  <- 100000 # number of simulations
ssize <- 100 # sample size
mu    <- 1
y     <- mu + rnorm(n = ssize*nsim, mean = 0, sd = 1) %>%
  matrix(., nrow = nsim, ncol = ssize)
y_bar <- rowMeans(y)

# Inspect
y_bar %>% head()


# Create grid for alpha
alpha <- seq(0.985, 1.001, by = 0.0005)
alpha %>%  head()

# Estimate mean error and mean squared error
mu_hat <- as.matrix(alpha) %*% t(as.matrix(y_bar))
me <- rowMeans(mu_hat - mu)
mse <- rowMeans((mu_hat - mu)^2)

# Make a table with results (per value of alpha)
simdata  <- data.frame(alpha = alpha, mse = mse, me = abs(me)) %>%
  as_tibble() %>%
  rename(`2) MSE` = mse, `1) Bias` = me) %>%
  gather(key, value, -alpha)

# Plot the results
alpha_star <-  mu^2/(mu^2 + 1/100*1)

ggplot(aes(x = y), data = data.frame(y = y[ , 1])) + geom_density() + geom_vline(xintercept = 1, linetype ="dashed", color = "red")

ggsave("../lectures/figures/example_distr_y.pdf", width = 8, height = 6, units = "cm")


ggplot(aes(x = alpha, y = value), data = simdata) + geom_line() + facet_wrap(.~key, ncol = 2, scales = "free") +
  geom_vline(xintercept = alpha_star, colour = "red", linetype = "longdash")+
  geom_vline(xintercept = 1, linetype = "dotted")
ggsave("../lectures/figures/example_muy.pdf", width = 18, height = 11, units = "cm")

# Verify that the answer is indeed 1 and alpha*
simdata %>%  filter(me == min(simdata$me))
simdata %>%  filter(mse == min(simdata$mse))



