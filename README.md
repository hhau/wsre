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

