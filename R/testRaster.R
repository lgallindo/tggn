#' Produces a rasterplot to test event detection tools.
#'
#' This function allows you to express your love of cats.
#' @param length Rasterplot length. Defaults to 60,000.
#' @param size Number of neurons or input signals. Defaults to 1,000.
#' @param activity Activity level from 0 to 1. Defaults to 0.1.
#' @param RNGdata A list of seed and RNG kind for random number generation. Defaults to the current workspace configuration.
#' @keywords neurons rasterplot
#' @export
#' @examples
#' cat_function()

testRaster <- function(length=60000, size=1000, activity=0.1, RNGdata=NULL) {

  if (is.null(RNGdata)) {
    rngseed <- .Random.seed
    rngkind <- RNGkind()
  } else {
    rngseed <- RNGdata$seed
    rngkind <- RNGdata$kind

    .Random.seed <- c(rngkind, rngseed)
  }

  raster <- matrix(rbinom(n=length*size, 1, activity), nrow=length, ncol=size, dimnames = list(Time = seq(1:length), Neuron = seq(1:size)))

  attr(raster, "RNGdata") <- list(seed=rngseed, kind=rngkind)

  return(raster)
}
