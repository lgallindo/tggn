#' Artifically injects an neural assembly event on a rasterplot.
#'
#' This function injects an neural assembly event on a rasterplot.
#' @param raster A matrix representing a rasterplot.
#' @param activity Activity level from 0 to 1. Defaults to 0.25.
#' @param confidence Confidence level from 0 to 1. Defaults to 0.8.
#' @param spread How should the signals be spread over time. Currently ignored.
#' @param RNGdata A list of seed and RNG kind for random number generation. Defaults to the current workspace configuration.
#' @keywords neurons rasterplot assembly event
#' @export
#' @examples
#' cat_function()

injectAssembly <- function(raster, neurons, activity=0.1, confidence=0.8, spread=0, RNGdata=data) {

  if (!is.matrix(raster)) stop("The raster parameter must be a matrix with Neurons on columns and Time on rows. Try to produce one using testRaster().")

  length <- dim(raster)[1]
  eventSize <- length(neurons)
  events <- sample.int(n=length, size=round(length*activity))

  for(e in 1:length(events)) {
    eventData <- rbinom(n=eventSize, 1, confidence)
    for(n in 1:eventSize) {
      raster[e, n]<-eventData[n]
    }
  }
}
