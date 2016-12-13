
#' group to median
#'
#' for preprocessing affymetrix SNP6 array
#' @param x data
#' @param f factor
#' @useDynLib wzard group_to_median_sorted_
#' @export
group.to.median.sorted <- function(x, f) .Call(group_to_median_sorted_, x, f)
