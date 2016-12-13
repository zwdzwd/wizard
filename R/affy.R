
#' group to median
#'
#' for preprocessing affymetrix SNP6 array
#' @param x data
#' @param f factor
#' @useDynLib wzard group_to_median_
#' @export
group.to.median <- function(x, f) .Call(group_to_median_, x, f)
