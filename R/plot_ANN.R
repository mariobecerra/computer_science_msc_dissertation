library(igraph)

gr_1 <- graph(
  c(c(1,2,
      1,3,
      1,4), 
    c(2,5,
      3,5,
      4,5)))

pdf("plot_ANN_01.pdf", 
    width = 3,
    height = 3) 

par(mai = c(0.0, 0.0, 0.0, 0.0))

plot(gr_1, 
     layout = 
       matrix(c(
         -2, 0, 
         0, 0.7,
         0, 0, 
         0, -0.7,
         2, 0), 
         byrow = T, 
         ncol = 2),
     vertex.label = c(expression(x[i]),
                      expression(a^(1)),
                      expression(a^(2)),
                      expression(a^(3)),
                      expression(y[i])),
     edge.label = c(expression(beta[1]^(1)),
                    expression(beta[1]^(2)),
                    expression(beta[1]^(3)),
                    expression(theta[1]),
                    expression(theta[2]),
                    expression(theta[3])
     ),
     vertex.size = 25, 
     vertex.color='white',
     vertex.label.cex = 1,
     vertex.label.color = 'black',
     vertex.frame.color = "black")

dev.off()



 







gr_2 <- graph(
  c(c(1,2,
      1,3,
      1,4), 
    c(2,5,
      2,6,
      3,5,
      3,6,
      4,5,
      4,6,
      5,7,
      6,7)))

pdf("plot_ANN_02.pdf", 
    width = 4,
    height = 4) 

par(mai = c(0.0, 0.0, 0.0, 0.0))

plot(gr_2, 
     layout = 
       matrix(c(
         -2, 0, 
         0, 1,
         0, 0, 
         0, -1,
         2, 0.5,
         2, -0.5,
         4, 0), 
         byrow = T, 
         ncol = 2),
     vertex.label = c(expression(x[i]),
                      expression(a[1]^(1)),
                      expression(a[1]^(2)),
                      expression(a[1]^(3)),
                      expression(a[2]^(1)),
                      expression(a[2]^(2)),
                      expression(y[i])),
     edge.label = c(expression(beta[1]^(1)),
                    expression(beta[1]^(2)),
                    expression(beta[1]^(3)),
                    expression(theta[1]^(1)),
                    expression(theta[1]^(2)),
                    expression(theta[2]^(1)),
                    expression(theta[2]^(2)),
                    expression(theta[3]^(1)),
                    expression(theta[3]^(2)),
                    expression(w[1]),
                    expression(w[2])
     ),
     vertex.size = 35, 
     vertex.color='white',
     vertex.label.cex = 1,
     vertex.label.color = 'black',
     vertex.frame.color = "black")

dev.off()
