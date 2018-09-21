# Computer Science master thesis

Latex code for my computer science master thesis.

### To compile on the cloud

Go to https://v2.overleaf.com/read/dscncbsfvjvf and compile.

### To compile locally

Must have `latex` and `biber` installed. Follow the following instructions:

1. Clone repository.

2. cd to `latex` directory.

3. Folder has a Makefile, so all you have to do is execute the command `make all`

### R scripts

- File `gradient_descent_example.R` implements gradient descent for logistic regression in simulated data. Generates file `GD_plots.pdf`, which is used in Figure 2.1 in Chapter 2.

- File `plot_mini_batch_gd_log_reg.R` implements mini-batch gradient descent for logistic regression in simulated data. Uses `mini_batch_gd_log_reg.cpp` file to compile C++ code via `Rcpp` package. Generates file `Mini-batch_GD_plots.png`, which is used in Figure 2.2 in Chapter 2.

- File `BBVI_logistic_regression.R` implements gradient ascent for Mean-fild Variational Approximation of posterior distribution of logistic regression in simulated data. Generates file `BBVI_plots.pdf`, which is used in Figure 2.3 in Chapter 2.

- File `plot_ANN.R` creates ANN diagrams. Generates files `plot_ANN_01.pdf`, `plot_ANN_02.pdf` and `plot_ANN_03.pdf`, which are used in Figures 2.4, 2.5 and 2.6 in Chapter 2.
