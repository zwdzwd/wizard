pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    stop("Optional package ", x, " not found. Please install before continue.")
  }
}
