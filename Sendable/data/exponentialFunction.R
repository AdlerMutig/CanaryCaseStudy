expon_prob <- function(obj){
  
  # The parameter of the exponential distribution is hardcoded to provide a
  # single distribution, could read this parameter in at some pont if user
  # needs control over the shape of the distribution
  mu = 0.3
  l = 1/mu
  pp = 1.0 - pexp(obj, rate=l) 
  return (pp)
}