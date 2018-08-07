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
                      expression(a[1]),
                      expression(a[2]),
                      expression(a[3]),
                      expression(y[i])),
     edge.label = c(expression(beta[1]^1),
                    expression(beta[1]^2),
                    expression(beta[1]^3),
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
                      expression(paste(a[1]^"[1]")),
                      expression(paste(a[2]^"[1]")),
                      expression(paste(a[3]^"[1]")),
                      expression(paste(a[1]^"[2]")),
                      expression(paste(a[2]^"[2]")),
                      expression(y[i])),
     edge.label = c(expression(beta[1]^1),
                    expression(beta[1]^2),
                    expression(beta[1]^3),
                    expression(theta[1]^1),
                    expression(theta[1]^2),
                    expression(theta[2]^1),
                    expression(theta[2]^2),
                    expression(theta[3]^1),
                    expression(theta[3]^2),
                    expression(w[1]),
                    expression(w[2])
     ),
     vertex.size = 30, 
     vertex.color='white',
     vertex.label.cex = 1,
     vertex.label.color = 'black',
     vertex.frame.color = "black")

dev.off()









gr_3 <- graph(
  c(
    # first layer
    c(1,5,
      1,6,
      1,7,
      2,5,
      2,6,
      2,7,
      3,5,
      3,6,
      3,7,
      4,5,
      4,6,
      4,7), 
    # second layer
    c(5, 8,
      5, 9,
      5, 10,
      5, 11,
      6, 8,
      6, 9,
      6, 10,
      6, 11,
      7, 8,
      7, 9,
      7, 10,
      7, 11),
    # third layer
    c(8, 12,
      9, 12,
      10, 12,
      11, 12)
  )
)

pdf("plot_ANN_03.pdf", 
    width = 9,
    height = 9) 

par(mai = c(0.0, 0.0, 0.0, 0.0))

plot(gr_3, 
     layout = 
       matrix(c(
         -2, 3, 
         -2, 1,
         -2, -1,
         -2, -3,
         0, 2,
         0, 0, 
         0, -2,
         2, 3,
         2, 1,
         2, -1,
         2, -3,
         4, 0), 
         byrow = T, 
         ncol = 2),
     vertex.label = c(expression(paste(x[i]^"(1)")),
                      expression(paste(x[i]^"(2)")),
                      expression(paste(x[i]^"(3)")),
                      expression(paste(x[i]^"(4)")),
                      expression(paste(a[1]^"[1]")),
                      expression(paste(a[2]^"[1]")),
                      expression(paste(a[3]^"[1]")),
                      expression(paste(a[1]^"[2]")),
                      expression(paste(a[2]^"[2]")),
                      expression(paste(a[3]^"[2]")),
                      expression(paste(a[4]^"[2]")),
                      expression(y[i])),
     edge.label = c(
       # layer 0 to 1
       expression(paste(theta["1,1"]^"[1]")),
       expression(paste(theta["1,2"]^"[1]")),
       expression(paste(theta["1,3"]^"[1]")),
       expression(paste(theta["2,1"]^"[1]")),
       expression(paste(theta["2,2"]^"[1]")),
       expression(paste(theta["2,3"]^"[1]")),
       expression(paste(theta["3,1"]^"[1]")),
       expression(paste(theta["3,2"]^"[1]")),
       expression(paste(theta["3,3"]^"[1]")),
       expression(paste(theta["4,1"]^"[1]")),
       expression(paste(theta["4,2"]^"[1]")),
       expression(paste(theta["4,3"]^"[1]")),
       # layer 1 to 2
       expression(paste(theta["1,1"]^"[2]")),
       expression(paste(theta["1,2"]^"[2]")),
       expression(paste(theta["1,3"]^"[2]")),
       expression(paste(theta["1,4"]^"[2]")),
       expression(paste(theta["2,1"]^"[2]")),
       expression(paste(theta["2,2"]^"[2]")),
       expression(paste(theta["2,3"]^"[2]")),
       expression(paste(theta["2,4"]^"[2]")),
       expression(paste(theta["3,1"]^"[2]")),
       expression(paste(theta["3,2"]^"[2]")),
       expression(paste(theta["3,3"]^"[2]")),
       expression(paste(theta["3,4"]^"[2]")),
       # layer 2 to 3
       expression(paste(theta["1,1"]^"[3]")),
       expression(paste(theta["2,1"]^"[3]")),
       expression(paste(theta["3,1"]^"[3]")),
       expression(paste(theta["4,1"]^"[3]"))
     ),
     vertex.size = 17, 
     vertex.color='white',
     vertex.label.cex = 1,
     vertex.label.color = 'black',
     vertex.frame.color = "black")

dev.off()
