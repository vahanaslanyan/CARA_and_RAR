# CARA and RAR Method Comparison

This code is to accompany "Bayesian response adaptive randomization for randomized clinical trials with continuous outcomes: the role of covariate adjustment." by Aslanyan et. al.
For cmdstan installation see https://mc-stan.org/users/interfaces/cmdstan
Change directories as needed. 
Edit simulation code to change between scenarios described in the paper. 

Longitudinal stan code mirrors the implementation of STAN code for longitudinal models from Julian Faraway's textbook ["Extending the linear model with R"](https://github.com/julianfaraway/rexamples/).

More details for the illustrative example can be found in the [TRAILBLAZER-ALZ 2 (Sims et al, 2023)](https://jamanetwork.com/journals/jama/article-abstract/2807533) trial.

## Getting Started

Clone this repo into your `Documents` folder.

```
cd ~/Documents
git clone git@github.com:vahanaslanyan/CARA_and_RAR.git
```

If placed in an alterante directory, modify the first line of `simulations.r` to reflect the current path:

```r
BASE_PATH<-"~/Documents/CARA_and_RAR/"
```

Run `simulations.r` using your preferred method (ex. RStudio). The genreated data will be saved into the `output/` directory. During the first run, an executable will be automatically created for each corresponding STAN file (3 total). If an error is encoutered, delete the executables before re-running.

To run different scenarios, modify the variables in `simulations.r`.

## Illustrative Examples

Run `trailblazer_long.R` for interim analyses based on MMRM (longitudinal), `trailblazer_reg.R` for interim analyses based on outcome change (regression). Modify code for different scenarios.

Instructions are identical to those for `simulations.r`.