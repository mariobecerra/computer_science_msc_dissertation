library(keras)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)

mnist = dataset_mnist()

plot_image <- function(img){
  out = img %>% 
    as_tibble() %>% 
    mutate(y = seq(nrow(.), 1, by = -1)) %>% 
    gather(x, value, -y) %>% 
    mutate(x = as.integer(gsub("V", "", x))) %>% 
    ggplot() +
    geom_raster(aes(x, y, fill = value)) +
    coord_equal() +
    scale_fill_continuous(low = "white", high = "black") +
    theme_void() +
    theme(legend.position = "none")
  return(out)
}

plot_images = function(imgs, ncol = 3){
  plots = lapply(imgs, plot_image)
  out = do.call("grid.arrange", c(plots, ncol = ncol))
  return(out)
}

# plot_image(mnist$train$x[1,,] )

set.seed(201812)
imgs = lapply(sample(1:60000, 100), function(i) mnist$train$x[i,,])

examples = plot_images(imgs, ncol = 10)

ggsave("mnist_data_examples.png", examples, dpi = 200, height = 5, width = 7, units = "cm")
