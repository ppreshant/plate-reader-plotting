tstf <- function(x = a1)
{
  rlang::as_string(ensym(x)) %>% str_c('_mean')
  # a %>% select({{x}})
  
}

fn_quoting <- function(ar1)
{
  ar1_q <- enexpr(ar1)
  print(ar1_q)
  class(ar1_q)
}

a %<>% group_by(a3) %>% mutate(across(c(a1, a2), lst(mean)))

plot_var_and_mean <- function(.dat = a, yvar = a2)
{
  yvarwmean <- rlang::parse_expr(rlang::as_string(ensym(yvar)) %>% str_c('_mean') )
  # class(yvarwmean) %>% print()
  
  ggplot(.dat, aes(a1, {{yvar}}, colour = a3)) +
    geom_point() +
    geom_line(aes(y = !!yvarwmean))
  
}
