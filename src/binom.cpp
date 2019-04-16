#include <Rcpp.h>
using namespace Rcpp ;
#include "stan_files/binom.hpp"
typedef rstan::stan_fit<stan_model, boost::random::ecuyer1988> stan_model_fit;

RCPP_MODULE(stan_fit4binom_mod) {


    class_<stan_model_fit>("model_binom")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &stan_model_fit::call_sampler)
    .method("param_names", &stan_model_fit::param_names)
    .method("param_names_oi", &stan_model_fit::param_names_oi)
    .method("param_fnames_oi", &stan_model_fit::param_fnames_oi)
    .method("param_dims", &stan_model_fit::param_dims)
    .method("param_dims_oi", &stan_model_fit::param_dims_oi)
    .method("update_param_oi", &stan_model_fit::update_param_oi)
    .method("param_oi_tidx", &stan_model_fit::param_oi_tidx)
    .method("grad_log_prob", &stan_model_fit::grad_log_prob)
    .method("log_prob", &stan_model_fit::log_prob)
    .method("unconstrain_pars", &stan_model_fit::unconstrain_pars)
    .method("constrain_pars", &stan_model_fit::constrain_pars)
    .method("num_pars_unconstrained", &stan_model_fit::num_pars_unconstrained)
    .method("unconstrained_param_names", &stan_model_fit::unconstrained_param_names)
    .method("constrained_param_names", &stan_model_fit::constrained_param_names)
    ;
}
