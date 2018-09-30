library(dplyr)
library(ggplot2)
library(gridExtra)

theme_set(theme_bw())

n = 5000000
p = 0.8
fill_alpha = 0.4
line_size = 0.1
xlim = c(-8, 8)
ylim = c(0, 0.29)

pi = rbinom(n = n, size = 1, p = p)

y1 = pi*rnorm(n, 5, 1) + (1 - pi)*rnorm(n, -5, 1)

dat_plot = tibble(y1 = pi*rnorm(n, 4, 1.2) + (1 - pi)*rnorm(n, -4, 1.2),
                  y2 = rnorm(n, 0, 3),
                  y3 = rnorm(n, 4, 1.5)) %>% 
  filter_all(all_vars(between(., xlim[1], xlim[2])))


plot_1 = dat_plot %>% 
  ggplot() +
  geom_density(aes(y1), color = "blue", fill = "blue", alpha = fill_alpha, size = line_size) +
  geom_density(aes(y3), color = "red", fill = "red", alpha = fill_alpha, size = line_size) +
  xlim(xlim) +
  ylim(ylim) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 

plot_2 = dat_plot %>% 
  ggplot() +
  geom_density(aes(y1), color = "blue", fill = "blue", alpha = fill_alpha, size = line_size) +
  geom_density(aes(y2), color = "red", fill = "red", alpha = fill_alpha, size = line_size) +
  xlim(xlim) +
  ylim(ylim) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 

plot_out = arrangeGrob(plot_1, plot_2, ncol = 2)

plot(plot_out)

