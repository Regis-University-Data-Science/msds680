pick.site <- function(epsilon = 0.1) {
  epsilon <- 0.99 * epsilon
  sites <- c('a', 'b', 'c')
  return(list(epsilon = epsilon, site = sites[2]))
}

t <- pick.site(epsilon = 0.2)
