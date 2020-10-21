## `wsre`: weighted-sample self-density ratio estimation

This package implements the methodology developed in [_A numerically stable algorithm for integrating Bayesian models using Markov melding_](https://arxiv.org/abs/2001.08038).
This package provides two functions:

1. `wsre()` for estimating the self-density ratio of interest,
2. `evaluate()` for evaluating the output of `wsre()`.

See the vignette for details on using these functions. 
I'd also suggest looking at the tests, particularly:

1. `test_evaluate` for additional examples on calling `evaluate()`, particularly in 2 or more dimension,
2. `test_wsre-main` for more simple example calls to `wsre()`.

## Dev workflow

If a `.stan` file has been modified, call:
```
devtools::load_all()
```
if not, proceed straight to:
```
devtools::document()
devtools::test()
devtools::install()
```

Sometimes the NAMESPACE file will get out of sync. It will need to be deleted,
and `devtools::document()` has to be run __before__ `devtools::load_all()`.
