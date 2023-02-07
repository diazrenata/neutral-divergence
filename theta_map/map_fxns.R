get_theta <- function(J, v) {
  
  2 * v * J
}

get_S <- function(J, theta) {
  
  hubbell::E.S(theta = theta, J = J)
  
}

get_S_from_pars <- function(J, v) {
  
  theta = get_theta(J = J, v = v)
  
  get_S(J = J, theta = theta)
}
