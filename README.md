# Computer Science master thesis

Latex code for my computer science master thesis. Supervised by [Alfredo Garbuno Iñigo](https://github.com/agarbuno).


### To compile document locally

Must have `latex` and `biber` installed. Follow the following instructions:

1. Clone repository.

2. cd to `latex` directory.

3. Folder has a Makefile, so all you have to do is execute the command `make all`

### To compile document on the cloud

Go to https://v2.overleaf.com/read/dscncbsfvjvf and compile. This version may not reflect most recent changes because I have to manually pull changes from the repository and I don't do this with every single commit.

### R scripts

- File `gradient_descent_example.R` implements gradient descent for logistic regression in simulated data. Generates file `GD_plots.pdf`.

- File `mini_batch_gradient_descent_example.R` implements mini-batch gradient descent for logistic regression in simulated data. Uses `mini_batch_gd_log_reg.cpp` file to compile C++ code via `Rcpp` package. Generates file `Mini-batch_GD_plots.png`.

- File `BBVI_logistic_regression.R` implements gradient ascent for Mean-field Variational Approximation of posterior distribution of logistic regression in simulated data. Generates file `BBVI_plots.pdf`.

- File `plot_ANN.R` creates ANN diagrams. Generates files `plot_ANN_01.pdf`, `plot_ANN_02.pdf` and `plot_ANN_03.pdf`.

- File `plot_KL_example.R` shows the difference between forward and reverse KL-divergence in a Gaussian mixture. Creates files `KL_example_1.pdf`, `KL_example_2.pdf` and `KL_example_3.pdf`.

- File `logistic_regression_AL_example.R` implements active learning in two logistic regression models with simulated data. Creates files `log_reg_AL_decision_boundary_plot.pdf` and `log_reg_AL_accuracies_plot.pdf`.
