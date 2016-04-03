# computing R, Q and Gamma
frqr <- function(I, t , W){
  # I is vector of indicators
  # t is the vector of period of life post-60
  # W is vector of weights
  
  SI <- sum(I*W)
  ST <- sum(t*W)
  SIT <- sum(I*W*t)
  SW <- sum(W)
  y <- c(SI, ST, SIT, SW)
  y
}