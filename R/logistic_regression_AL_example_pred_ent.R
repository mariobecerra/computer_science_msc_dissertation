library(MASS)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gridExtra)

theme_set(theme_bw(base_size = 14))
seed = 1234

# Functions ---------------------------------------------------------------



sigmoid = function(w){
  return(1/(1 + exp(-w)))
}


ones = function(P){
  return(rep(1.0, P))
}


create_data_ex_1 = function(N, real_theta, seed = 0){
  set.seed(seed)
  x1 = rnorm(N)
  x2 = rnorm(N)
  pr = sigmoid(real_theta[1]*x1 + real_theta[2]*x2)
  y = rbinom(N, 1, pr)
  out = cbind(x1, x2, y) %>% 
    as_tibble()
  return(out)
}

create_data_ex_2 = function(N, real_theta, seed = 0){
  set.seed(seed)
  P = length(real_theta)
  X = mvrnorm(N, mu = rep(0, P), Sigma = diag(rep(1, P)))
  y = rbinom(N, 1, sigmoid(X %*% real_theta))
  out = X %>% 
    as_tibble() %>% 
    set_names(paste0("x", 1:P)) %>% 
    mutate(y = y)
  return(out)
}



create_initial_train <- function(y, n = 5, seed = 2018){
  set.seed(seed)
  
  y_temp = tibble(y = y) %>% 
    mutate(ix = 1:nrow(.)) %>% 
    arrange(y)
  
  indices_df = map_df(0:1, function(i){
    filt = y_temp %>% 
      filter(y == i)
    out = tibble(y = rep(as.integer(i), n),
                 ix = sample(filt$ix, n))
    return(out)
  })  
  return(indices_df$ix)
}



create_initial_pool_train_val <- function(y_all, n = 5, seed = 2018){
  set.seed(seed)
  ix_train = create_initial_train(y_all, n, seed)
  available_idx = setdiff(1:length(y_all), ix_train)
  shuffled_indices = sample(available_idx, length(available_idx))
  ix_val = shuffled_indices[1:300]
  ix_pool = shuffled_indices[301:length(shuffled_indices)]
  # ix_pool = shuffled_indices[1:length(shuffled_indices)]  
  return(list(
    ix_train = ix_train,
    ix_val = ix_val,
    ix_pool = ix_pool
  ))
}


# Decision boundary example -----------------------------------------------

# Create and plot data
final_n = 80

real_theta = c(1, 1)

dat_1 = create_data_ex_1(1000, real_theta, seed = seed) %>% 
  mutate(ix = 1:nrow(.))

# mod_0 = glm(y ~ x1+x2-1, data = dat_1, family = "binomial")

gg_1.1 = dat_1 %>% 
  mutate(y = as.character(dat_1$y)) %>% 
  ggplot() +
  geom_point(aes(x1, x2, color = y, shape = y), size = 1.2, alpha = 0.8) +
  geom_abline(slope = -real_theta[1]/real_theta[2], size = 0.3) +
  # geom_abline(slope = - mod_0$coefficients[1]/mod_0$coefficients[2], linetype = "F1", size = 0.7) +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position='none') +
  xlab("") +
  ylab(expression(x[2]))

# Random sample

set.seed(seed)
dat_1_random = sample_n(dat_1, final_n)
mod_1 = glm(y ~ x1+x2-1, data = dat_1_random, family = "binomial")


gg_1.2 = dat_1 %>% 
  ggplot() +
  geom_point(aes(x1, x2), size = 0.1, alpha = 0.4, color = "dark grey") +
  geom_abline(slope = -real_theta[1]/real_theta[2], size = 0.3) +
  geom_abline(slope = - mod_1$coefficients[1]/mod_1$coefficients[2], linetype = "F1", size = 0.6) +
  geom_point(data = dat_1_random %>% 
               mutate(y = as.character(y)),
             aes(x1, x2, color = y, shape = y), size = 1.8, alpha = 1) +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position='none') +
  xlab(expression(x[1])) +
  ylab("")

# Active learning

# Initial set of 10 training examples
initial_pool_train_val = create_initial_pool_train_val(dat_1$y, 5, seed)
ix_train = initial_pool_train_val$ix_train
ix_pool =  initial_pool_train_val$ix_pool

n_iter_1 = final_n - length(ix_train)


for(i in 1:n_iter_1){
  mod = glm(y ~ . -1, data = slice(dat_1, ix_train), family = "binomial")
  
  preds_pool = predict(mod, newdata = slice(dat_1, ix_pool), type = "response")
  
  ix_new_data = tibble(ix = ix_pool, p = preds_pool) %>% 
    mutate(pred_ent = - p*log(p) - (1-p)*log(1-p) ) %>% 
    arrange(desc(pred_ent)) %>% 
    slice(1) %>% 
    pull(ix)
  
  ix_pool = setdiff(ix_pool, ix_new_data)
  ix_train = c(ix_train, ix_new_data)
  
}

# Only the points chosen by the acquisition function
chosen_AL_points = setdiff(ix_train, initial_pool_train_val$ix_train)

dat_1_AL = slice(dat_1, ix_train)

mod_2.2 = glm(y ~ x1+x2-1, data = dat_1_AL, family = "binomial")


gg_1.3 = dat_1 %>% 
  ggplot() +
  geom_point(aes(x1, x2), size = 0.1, alpha = 0.4, color = "dark grey") +
  geom_abline(slope = -real_theta[1]/real_theta[2], size = 0.3) +
  geom_abline(slope = - mod_2.2$coefficients[1]/mod_2.2$coefficients[2], linetype = "F1", size = 0.6) +
  geom_point(data = dat_1 %>% 
               slice(chosen_AL_points) %>% 
               mutate(y = as.character(y)),
             aes(x1, x2, color = y, shape = y), size = 1.8, alpha = 1) +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = 'none') +
  xlab("") +
  ylab("")


out_plot_1 = arrangeGrob(gg_1.1, gg_1.2, gg_1.3, 
                         ncol = 3)

plot(out_plot_1)

ggsave(out_plot_1, filename = "log_reg_AL_decision_boundary_plot_predictive_entropy.pdf", dpi = 250, width = 24, height = 8, units = "cm")


# Accuracy example ------------------------------------------------------------------

set.seed(seed)
real_theta = c(runif(10, -5, 5))

dat_2 = create_data_ex_2(1000, real_theta, seed = 0)

# Initial set of 10 training examples
initial_n = 20


n_train_final = 200
n_iter = n_train_final - initial_n

n_runs = 50

### Random acquisition

accuracies_random_all = map_df(1:n_runs, function(k){
  cat("Iter", k, "\n")
  
  initial_pool_train_val = create_initial_pool_train_val(dat_2$y, floor(initial_n/2), seed+k)
  
  accuracies_random = tibble(iter = 1:n_iter) %>% 
    mutate(acc = NA)
  
  ix_pool = initial_pool_train_val$ix_pool
  ix_train = initial_pool_train_val$ix_train
  ix_val = initial_pool_train_val$ix_val
  
  for(i in 1:n_iter){
    mod = glm(y ~ . -1, data = slice(dat_2, ix_train), family = "binomial")
    
    preds_val = predict(mod, newdata = slice(dat_2, ix_val), type = "response")
    acc = mean(ifelse(preds_val > 0.5, 1, 0) == dat_2$y[ix_val])
    accuracies_random$acc[i] = acc
    
    ix_new_data = sample(ix_pool, 1)
    
    ix_pool = setdiff(ix_pool, ix_new_data)
    ix_train = c(ix_train, ix_new_data)
    
  }
  cat("\n\n")
  
  out = accuracies_random %>% 
    mutate(run = k)
  
  return(out)
})


### Active Learning

accuracies_AL_all = map_df(1:n_runs, function(k){
  cat("Iter", k, "\n")
  initial_pool_train_val = create_initial_pool_train_val(dat_2$y, floor(initial_n/2), seed+k)
  
  accuracies_AL = tibble(iter = 1:n_iter) %>% 
    mutate(acc = NA)
  
  ix_pool = initial_pool_train_val$ix_pool
  ix_train = initial_pool_train_val$ix_train
  ix_val = initial_pool_train_val$ix_val
  
  print(ix_train)
  
  for(i in 1:n_iter){
    mod = glm(y ~ . -1, data = slice(dat_2, ix_train), family = "binomial")
    
    preds_val = predict(mod, newdata = slice(dat_2, ix_val), type = "response")
    
    acc = mean(ifelse(preds_val > 0.5, 1, 0) == dat_2$y[ix_val])
    accuracies_AL$acc[i] = acc
    
    preds_pool = predict(mod, newdata = slice(dat_2, ix_pool), type = "response")
    
    ix_new_data = tibble(ix = ix_pool, p = preds_pool) %>% 
      mutate(pred_ent = - p*log(p) - (1-p)*log(1-p)) %>% 
      arrange(desc(pred_ent)) %>% 
      slice(1) %>% 
      pull(ix)
    
    ix_pool = setdiff(ix_pool, ix_new_data)
    ix_train = c(ix_train, ix_new_data)
    
  }
  
  out = accuracies_AL %>% 
    mutate(run = k)
  cat("\n\n")
  return(out)
  
})


### Plot

plot_colors = c("blue", "red")
plot_labels = c("Active Learning", "Random")
plot_legend_name = "Type of\nacquisition"

out_plot_2 = accuracies_random_all %>% 
  mutate(type = "random") %>% 
  group_by(iter, type) %>% 
  summarize(accuracy = mean(acc), p25 = quantile(acc, 0.25), p75 = quantile(acc, 0.75)) %>% 
  ungroup() %>% 
  bind_rows(
    accuracies_AL_all %>% 
      mutate(type = "AL") %>% 
      group_by(iter, type) %>% 
      summarize(accuracy = mean(acc), p25 = quantile(acc, 0.25), p75 = quantile(acc, 0.75)) %>% 
      ungroup()
  ) %>% 
  mutate(n_examples = initial_n + iter) %>% 
  ggplot(aes(x = n_examples)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = type), alpha = 0.1) +
  geom_line(aes(y = accuracy, color = type)) +
  scale_fill_manual(values = plot_colors, 
                    name = plot_legend_name,
                    labels = plot_labels) +
  scale_color_manual(values = plot_colors, 
                     name = plot_legend_name,
                     labels = plot_labels) +
  xlab("Number of data points") +
  ylab("Accuracy")


ggsave(out_plot_2, filename = "log_reg_AL_accuracies_plot_predictive_entropy.pdf.pdf", dpi = 250, width = 24, height = 12, units = "cm")


