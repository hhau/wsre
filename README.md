## `wsre`: weighted-sample self-density ratio estimation

This package implements the methodology developed in [_A numerically stable algorithm for integrating Bayesian models using Markov melding_](https://arxiv.org/abs/2001.08038).
This package provides two functions:
1. `wsre()` for estimating the self-density ratio of interest,
2. `evaluate()` for evaluating the output of `wsre()`.

See the vignette for examples on using these functions. 
I'd also suggest looking at the tests, particularly

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

## Development notes to self

At the moment, upgrading `StanHeaders` leads to a compilation error, stick with     2.18.1. (__NOT__: 2.18.1-10).

There are a few different tasks I want to accomplish here

- All the ways to estimate weighted unnormalised density estimates
    - ~specify a bunch of means that span the region of interest?~
    - use the naive KDE estimate (need it anyway) to estimate what the wf_means need to be so that we learn about the correct regions
        - Computing this is seriously nontrivial
- ~Sensibly compute weighted self ratio estimates as a function of arbitrarily many unnormalised density estimates~
    - ~lists of lists - lots and lots of closures~, potentially very slow? Rcpp can minimise function evaluation time but not overhead
        - No way of knowing if this is slow without something to compare it to.
- ~Sensible object to save to disk to reuse later in a melding context~
  - ~Eval methods~
- Plots and diagnostics
  - Should the Monte Carlo stuff be in here? I'm not sure it's useful

There are some technical problems I don't yet quite know the answer too:

- At the moment, I think i'm going to need to specific a data program that has a `data` block with certain parameter names? 
    - `wf_mean`, `wf_sd`, `wf_exp` etc (last one is useful to turn things off)
    - Recompile every time a new model is added? Doesn't seem unreasonable, probably not going to happen too often.
- ~`Rcpp` code and `Stan` code in one package? How do / how not to have one ruin the other?~

## Package notes

- `wsre` stands for _weighted self ratio estimation_. We are trying to estimate the ratio of an unknown probability density function `r(x_nu, x_de) = p(x_nu) / p(x_de)`.
    - It is _weighted_ in two important ways.
        1. The samples used to estimate `r(x_nu, x_de)` come from a weighted version of the target density, so that we can learn out the tails faster.
        1. The resulting ratio estimate is a weighted sum of other ratio estimates.
