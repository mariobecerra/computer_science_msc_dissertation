library(tidyverse)
library(gridExtra)
library(scales)

theme_set(theme_bw(base_size = 12))

###################################
###################################
### Logistic Regression
###################################
###################################

N <- 1000
theta_0 <- -5
theta_1 <- 5

set.seed(126774)
data <- tibble(x = rnorm(N),
               z = theta_0 + theta_1*x,
               pr = 1/(1 + exp(-z)),
               y = rbinom(1000, 1, pr))

mod_glm <- glm(y ~ x, data = data, family = "binomial")


h <- function(x){
  return(1/(1 + exp(-x)))
}

gradient <- function(data, thetas){
  n <- nrow(data)
  const <- rep(1, nrow(data))
  bx <- thetas[1]*const + thetas[2]*data$x
  li <- data$y - h(bx)
  g1 <- -2*sum(li)/n
  g2 <- -2*sum(li*data$x)/n
  return(c(g1, g2))
}


deviance <- function(thetas, data = data){
  n <- nrow(data)
  const <- rep(1, nrow(data))
  bx <- thetas[1]*const + thetas[2]*data$x
  hbx <- h(bx)
  aux <- (1 - data$y)*log(1 - hbx)
  aux2 <- ifelse(is.nan(aux), 0, aux)
  li <- data$y*log(hbx) + aux2
  d <- -2*sum(li)/n
  return(d)
}

alpha = 0.1
max_it <- 50000
data_gradient_descent <- tibble(it = 1:max_it,
                                theta_0 = rep(0, max_it),
                                theta_1 = rep(0, max_it),
                                gradient_norm = rep(0, max_it),
                                deviance = rep(0, max_it),
                                alpha = rep(0, max_it))

i = 0
thetas <- c(0, 0)
g <- gradient(data, thetas)
g_norm <- sqrt(sum(g^2))
while(i < max_it){
  i = i + 1
  g_norm_0 <- g_norm # old gradient norm
  g <- gradient(data, thetas)
  g_norm <- sqrt(sum(g^2))
  g_unit <- g/g_norm
  data_gradient_descent$theta_0[i] <- thetas[1]
  data_gradient_descent$theta_1[i] <- thetas[2]
  data_gradient_descent$gradient_norm[i] <- g_norm
  data_gradient_descent$deviance[i] <- deviance(thetas, data)
  data_gradient_descent$alpha[i] <- alpha
  thetas_old = thetas
  thetas <- thetas - alpha*g
  # Compute the norm of the theta differences
  dif_thetas_norm <- sum((thetas - thetas_old)^2)
  flag_1 = g_norm < 1e-8
  flag_2 = dif_thetas_norm < 1e-8
  if(flag_1 | flag_2) {
    if(flag_1) cat("\n\nEnded because of convergence in gradient.\n\n")
    if(flag_2) cat("\n\nEnded because of convergence in thetas values.\n\n")
    break
  }
  alpha = 0.1
  if(i %% 100 == 1){
    cat("It: ", i, 
        "\n\ttheta_0: ", thetas[1], 
        "\n\ttheta_1: ", thetas[2],
        "\n\tgradient_norm: ", g_norm, 
        "\n\tDirection: ", g_unit[1], g_unit[2],
        "\n\talpha: ", alpha,
        "\n\n")
  }
  
}


data_gradient_descent <- data_gradient_descent[1:i,]

iter_loss_plot = data_gradient_descent %>% 
  ggplot(aes(it, deviance)) +
  geom_line(size = 0.5) +
  xlab("Iteration") +
  ylab("Loss function") +
  scale_x_continuous(label = comma)


iter_theta_plot =  data_gradient_descent %>% 
  ggplot(aes(theta_0, theta_1)) +
  geom_vline(xintercept = mod_glm$coefficients[1], color = 'red', linetype = 'dotted') +
  geom_hline(yintercept = mod_glm$coefficients[2], color = 'red', linetype = 'dotted') +
  xlab(expression(theta[0])) +
  ylab(expression(theta[1])) +
  geom_segment(
    aes(
      xend = c(tail(theta_0, n = -1), NA),
      yend = c(tail(theta_1, n = -1), NA)
    ),
    size = 0.2,
    color = '#919191'
  ) +
  geom_point(size = 0.01, alpha = 0.6, color = 'black') +
  geom_point(aes(x, y),
             data = tibble(x = mod_glm$coefficients[1],
                           y = mod_glm$coefficients[2]),
             # shape = 'x',
             color = 'red',
             size = 1) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(label = comma)


out_plot = arrangeGrob(iter_loss_plot, iter_theta_plot, 
                       ncol = 2)

# plot(out_plot)

ggsave(out_plot, filename = "GD_plots.pdf", dpi = 250, width = 15, height = 6, units = "cm")
