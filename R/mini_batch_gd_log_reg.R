library(gridExtra)
library(Rcpp)
library(tidyverse)

sourceCpp("sgd.cpp")

log_reg_minibatch <- function(X, y, minibatch_size = 15, 
                              max_it = 3000, 
                              initial_point = NA, 
                              seed = 201802,
                              alpha = 0.001,
                              g_norm_tol = 1e-8,
                              beta_diff_norm_tol = 1e-8,
                              verbose = F){
  
  # Parameters:
  # X: covariate data
  # y: response variable
  # minibatch_size: Minibatch size
  # max_it: maximum number of iterations of the algorithm
  # initial_point: initial point for the beta parameters
  # seed: seed for the data shuffling face
  # alpha: learning rate
  # g_norm_tol: gradient norm tolerance
  # beta_diff_norm_tol: beta difference norm tolerance
  # verbose: whether to print each iteration number or not
  
  data_matrix = as.matrix(cbind(X, y)) # Creates a data matrix
  n <- nrow(data_matrix) # number of observations
  p <- ncol(data_matrix) # number of parameters (including intercept)
  
  # Default initial point
  if(is.na(initial_point[1])) initial_point = rep(0.0, p)
  
  # Number of iterations according to minibatch size
  if(n %% minibatch_size == 0) {
    num_iters = floor(n/minibatch_size)
  } else {
    num_iters = floor(n/minibatch_size) + 1
  }
  
  # Two daa frames that keep track of the values of the parameters during the execution of the algorithm.
  # This may consume a lot of memory if the data has too many observations and
  # the max_it parameter is too big
  betas_df <- as.data.frame(matrix(rep(0, max_it*num_iters*p), ncol = p))
  names(betas_df) <- paste0("beta_", 0:(p-1))
  iteration_values <- tibble(
    epoch = rep(0, max_it*num_iters),
    obs = 1:(max_it*num_iters),
    gradient_norm = rep(0, max_it*num_iters)) %>% 
    bind_cols(betas_df)
  
  i = 0
  betas <- initial_point
  # Start the algorithm
  while(i < max_it){
    i = i + 1
    if(verbose) {
      if(i %% 1000 == 1){
        cat("Iter:", i, "\n")
        cat("Betas:", betas, "\n")
        cat("\n")
      }
    }
    # Shuffle data
    set.seed(seed)
    shuffle_ix <- sample(nrow(data_matrix))
    shuffled_data = data_matrix[shuffle_ix,]
    # Compute beta parameters for one epoch
    epoch_betas <- epoch_update(shuffled_data, betas, alpha, i, minibatch_size)
    
    # Update beta parameters
    betas_old <- betas
    betas <- epoch_betas$betas
    # Save values of each iteration in the output dataframe
    epoch_values_temp <- as.data.frame(epoch_betas$epoch_values)
    names(epoch_values_temp) <- names(iteration_values)
    iteration_values[((i-1)*num_iters + 1):((i)*num_iters),] <- epoch_values_temp
    # Compute gradient norm
    g_norm <- epoch_betas$epoch_values[num_iters, 3]
    # Compute the norm of the beta differences
    dif_betas_norm <- sum((betas - betas_old)^2)
    # If the gradient norm is close to zero, or the parameters hardly change, exit the algorithm
    if(g_norm < g_norm_tol | dif_betas_norm < beta_diff_norm_tol) break
  }
  
  # Keep only the values of the valid iterations
  iteration_values <- iteration_values[1:(i*num_iters),]
  iteration_values$it <- 1:nrow(iteration_values)
  
  return(list(
    iteration_values = iteration_values,
    betas = betas
  ))
}

# Function that plots the values of the parameters in each iteration.
# It only works for simple linear regression (2 parameters).
plot_minibatches_2_params <- function(lm_minibatch
                                      # beta_0, beta_1, coef_1, coef_2
){
  data <- lm_minibatch$iteration_values
  if(length(lm_minibatch$beta) == 2) {
    n = nrow(data)
    val_1 = data$beta_0[n]
    val_2 = data$beta_1[n]
    gg <- data %>% 
      ggplot(aes(beta_0, beta_1)) +
      xlab("Beta 0") +
      ylab("Beta 1") +
      geom_path(size = 0.1, color = 'black') +
      geom_point(size = 0.01, color = 'black', alpha = 0.2) +
      geom_point(aes(x, y),
                 data = tibble(x = val_1,
                               y = val_2),
                 shape = 'x',
                 size = 5,
                 color = 'blue') +
      theme_bw()
    return(gg)
  } else{
    return("Error")
  }
}



N <- 1000
theta_0 <- -5
theta_1 <- 5

set.seed(124362)
data <- tibble(x = rnorm(N),
               z = theta_0 + theta_1*x,
               pr = 1/(1 + exp(-z)),
               y = rbinom(1000, 1, pr))

mod_gradient_descent <- log_reg_minibatch(data[,"x"], 
                                          data$y, 
                                          minibatch_size = 1000, 
                                          max_it = 5000, 
                                          alpha = 0.1,
                                          initial_point = c(0, 0),
                                          verbose = T) 

plot_minibatches_2_params(mod_gradient_descent)





mod_sgd <- log_reg_minibatch(data[,"x"], 
                             data$y, 
                             minibatch_size = 1, 
                             max_it = 5000, 
                             alpha = 0.1,
                             initial_point = c(0, 0),
                             verbose = T) 

plot_minibatches_2_params(mod_sgd)





plots_size <- lapply(0:8, function(i){
  mb_size = 2^i
  mod_minibatch = log_reg_minibatch(data[,"x"], 
                    data$y, 
                    minibatch_size = 2^i, 
                    max_it = 5000, 
                    alpha = 0.1,
                    initial_point = c(0, 0),
                    verbose = T) 
  gg <- plot_minibatches_2_params(mod_minibatch) +
    ggtitle(paste("Size:", mb_size))
  return(gg)
})

# Didn't bother to look for a more elegant way to do this
grid.arrange(plots_size[[1]],
             plots_size[[2]],
             plots_size[[3]],
             plots_size[[4]],
             plots_size[[5]],
             plots_size[[6]],
             plots_size[[7]],
             plots_size[[8]],
             plots_size[[9]],
             ncol = 3)
