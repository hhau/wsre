## `wsre`: weighted-sample self-density ratio estimation

This package implements the methodology developed in [_A numerically stable algorithm for integrating Bayesian models using Markov melding_](https://arxiv.org/abs/2001.08038).
This package provides two functions:

1. `wsre()` for estimating the self-density ratio of interest,
2. `evaluate()` for evaluating the output of `wsre()`.

See the vignette for details on using these functions. 
I'd also suggest looking at the tests, particularly:

1. `test_evaluate` for additional examples on calling `evaluate()`, particularly in 2 or more dimension,
2. `test_wsre-main` for more simple example calls to `wsre()`.

## Development notes 

This is going to use:
https://github.com/mbertolacci/stanmodularise
to avoid using rstantools with Rcpp code (too hard).

## Dev workflow

If a `.stan` file has been modified, call:
```
stanmodularise::modularise_stan_files()
devtools::load_all()
```
if not, proceed straight to:
```
devtools::document()
devtools::test()
devtools::install()
```
It is somewhat annoying that `devtools::install()` will recompile the Stan 
files, even when they have been unmodified since last install. However, the
use of `stanmodularise` halves the compilation time when making pure `R` changes
to the package (I think).

Sometimes the NAMESPACE file will get out of sync. It will need to be deleted,
and `devtools::document()` has to be run __before__ `devtools::load_all()`.
