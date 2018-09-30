# This script creates a mixture of 2 Gaussians and computes the reverse and forward KL divergences
# of the mixture and two different approximating distributions.
# Saves two plots in PDF format.
# Takes about 5 minutes to run on a Macbook Pro 2017 with 8 cores.
library(parallel)
library(FNN)
library(dplyr)
library(purrr)
library(ggplot2)

theme_set(theme_bw())

n = 10000000
prob = 0.8
fill_alpha = 0.4
line_size = 0.1
xlim = c(-8, 8)
ylim = c(0, 0.29)

pi = rbinom(n = n, size = 1, p = prob)

p = pi*rnorm(n, 5, 1) + (1 - pi)*rnorm(n, -5, 1)

dat_plot = tibble(p = pi*rnorm(n, 4, 1.2) + (1 - pi)*rnorm(n, -4, 1.2),
                  q1 = rnorm(n, 0, 3),
                  q2 = rnorm(n, 4, 1.5)) %>% 
  filter_all(all_vars(between(., xlim[1], xlim[2])))


### Compute KL divergences
n_cores = detectCores()

commands = c(
  # Forward KL
  "KL.divergence(dat_plot$p, dat_plot$q1, k=1)",
  
  # Reverse KL
  "KL.divergence(dat_plot$q1, dat_plot$p, k=1)",
  
  # Forward KL
  "KL.divergence(dat_plot$p, dat_plot$q2, k=1)",
  
  # Reverse KL
  "KL.divergence(dat_plot$q2, dat_plot$p, k=1)"
)


kl_divs = mclapply(commands, function(x) eval(parse(text = x))) %>% 
  simplify()
# Using 4 cores takes half the time than doing it sequentially
# [1] 0.8769344 1.4581808 2.1687245 0.2680020


## Tibbles to position the text in the plot
data_text_1 = tibble(
  text = paste0(c("Forward KL = ", "Reverse KL = "), round(kl_divs[1:2], 3)),
  x = c(-4, -4),
  y = c(0.2, 0.18)
)

data_text_2 = tibble(
  text = paste0(c("Forward KL = ", "Reverse KL = "), round(kl_divs[3:4], 3)),
  x = c(-4, -4),
  y = c(0.2, 0.18)
)


## Plots
plot_1 = dat_plot %>% 
  ggplot() +
  geom_density(aes(p), color = "blue", fill = "blue", alpha = fill_alpha, size = line_size) +
  geom_density(aes(q2), color = "red", fill = "red", alpha = fill_alpha, size = line_size) +
  geom_text(data = data_text_1,
            aes(x, y, label = text)) +
  xlim(xlim) +
  ylim(ylim) +
  xlab(expression(theta)) +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 

plot_2 = dat_plot %>% 
  ggplot() +
  geom_density(aes(p), color = "blue", fill = "blue", alpha = fill_alpha, size = line_size) +
  geom_density(aes(q1), color = "red", fill = "red", alpha = fill_alpha, size = line_size) +
  geom_text(data = data_text_2,
            aes(x, y, label = text)) +
  xlim(xlim) +
  ylim(ylim) +
  xlab(expression(theta)) +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 


# Save plots
ggsave("KL_example_1.pdf", plot_1, width = 12, height = 12, units = "cm")
ggsave("KL_example_2.pdf", plot_2, width = 12, height = 12, units = "cm")


