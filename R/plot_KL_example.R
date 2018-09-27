
plot_1 = dat_plot %>% 
  ggplot() +
  geom_density(aes(y1), color = "blue", fill = "blue", alpha = fill_alpha) +
  geom_density(aes(y3), color = "red", fill = "red", alpha = fill_alpha) +
  xlim(-7, 7) +
  ylim(0, 0.35) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 


plot_2 = dat_plot %>% 
  ggplot() +
  geom_density(aes(y1), color = "blue", fill = "blue", alpha = fill_alpha) +
  geom_density(aes(y2), color = "red", fill = "red", alpha = fill_alpha) +
  xlim(-7, 7) +
  ylim(0, 0.35) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_blank(), # Remove x axis tick labels
    axis.text.y = element_blank(), # Remove y axis tick labels
    axis.ticks = element_blank()   # Remove ticks 
  ) 

plot_out = arrangeGrob(plot_1, plot_2, ncol = 2)

plot(plot_out)

