# Automatically generated
loadModule('stan_fit4weighted_normal_mod', TRUE)
.stan_modules <-
list(weighted_normal = list(name = "weighted_normal", stan_file = "src/stan_files/weighted-normal.stan", 
    module_name = "stan_fit4weighted_normal_mod"))
.stan_models <- lapply(.stan_modules, function(input) {
  stanfit <- rstan::stanc(
   input$stan_file,
   allow_undefined = TRUE,
   obfuscate_model_name = FALSE
  )
  stanfit$model_cpp <- list(
   model_cppname = stanfit$model_name,
   model_cppcode = stanfit$cppcode
  )
  do.call(
    methods::new,
    args = c(
      stanfit[-(1:3)],
      Class = 'stanmodel',
      mk_cppmodule = function(x) get(paste0('model_', input$name))
    )
  )
})
