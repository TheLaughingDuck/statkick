#' @title Generate uniformly random numbers
#'
#' @description This method uses an implementation of Linear Congruential pseudorandom Generators (LCG), with constants chosen based on the reference (listed below).
#'
#' @param n A positive integer specifying the number of values to generate.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#'
#' @return A uniformly random number.
#' @export unif_random
#'
#' @references Steele, Guy; Vigna, Sebastiano (15 January 2020). "Computationally easy, spectrally good multipliers for congruential pseudorandom number generators". [arXiv: 2001.05304](https://arxiv.org/abs/2001.05304)
#'

unif_random <- function(n, min=0, max=1){
  # LCG integers
  m <- 2**64
  a <- 17380933483125451205
  c <- 0

  output <- c()
  for(i in 1:n){
    seed <- as.numeric(Sys.time())
    x <- ((a*seed + c) %% m)/ m

    output <- c(output, x)
  }

  return(output)

}
