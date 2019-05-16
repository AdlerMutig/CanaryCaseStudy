beta_prob <- function(obj){
  
  # The parameter of the exponential distribution is hardcoded to provide a
  # single distribution, could read this parameter in at some pont if user
  # needs control over the shape of the distribution
  aa = 2.0;  # alpha parameter of beta distribution  (2.0)
  bb = 1.0;  # beta parameter of beta distribution   (1.0)


  pp = 1.0 - dbeta(x, shape1=a, shape2=b)
  return (pp)
}