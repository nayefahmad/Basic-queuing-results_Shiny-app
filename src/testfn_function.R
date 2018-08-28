

# define test function: 
testfn <- function(int.vec) {
      quantile(int.vec, probs = c(0.9, .99))
}

# test the fn: 
testfn(rnorm(100))
