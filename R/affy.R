
#' group to median
#'
#' for preprocessing affymetrix SNP6 array
#' @param x data
#' @param f factor
#' @useDynLib wzard group_to_median_sorted_
#' @export
group.to.median.sorted <- function(x, f) .Call(group_to_median_sorted_, x, f)

#' group to max
#'
#' for preprocessing affymetrix SNP6 array
#' @param x data
#' @param f factor
#' @useDynLib wzard group_to_max_sorted_
#' @export
group.to.max.sorted <- function(x, f) .Call(group_to_max_sorted_, x, f)

#' read signal from one CEL file
#'
#' @param cel.fn path to CEL
#' @return SignalSet
#' @importFrom affyio read.celfile
#' @export
read.cel.signals <- function(cel.fn) {
  cel.raw <- read.celfile(cel.fn, intensity.means.only=TRUE)
  y <- matrix(cel.raw[['INTENSITY']][['MEAN']], nrow=cel.raw[['HEADER']][['CEL dimensions']][1])

  snp.probes <- getBuiltInData('snp6.snp.probes')
  cn.probes <- getBuiltInData('snp6.cn.probes')
  probe2allele <- getBuiltInData('snp6.probe2allele')
  allele2locus <- getBuiltInData('snp6.allele2locus')

  cn <- setNames(y[cbind(cn.probes$PROBE_X_POS+1, cn.probes$PROBE_Y_POS+1)], cn.probes$PROBESET_ID)
  snp <- setNames(y[cbind(snp.probes$PROBE_X_POS+1, snp.probes$PROBE_Y_POS+1)], snp.probes$PROBE_UID)
  snp.median <- group.to.median.sorted(snp, probe2allele)
  snp.median.max <- group.to.max.sorted(snp.median, allele2locus)

  structure(list(cn = cn, snp = snp, snp.median = snp.median, snp.median.max = snp.median.max), class='SignalSet')
}

#' get copy number signal from SignalSet
#'
#' @param sset SignalSet
#' @return sorted GRanges with signals
#' @import GenomicRanges
#' @export
sset.cn.signal <- function(sset) {
  snp.pos <- getBuiltInData('snp6.snp.pos')
  cn.pos <- getBuiltInData('snp6.cn.pos')
  GenomicRanges::mcols(snp.pos) <- NULL
  GenomicRanges::mcols(cn.pos) <- NULL
  GenomicRanges::mcols(snp.pos)$signal <- sset$snp.median.max[match(names(snp.pos), names(sset$snp.median.max))]
  GenomicRanges::mcols(cn.pos)$signal <- sset$cn[match(names(cn.pos), names(sset$cn))]
  GenomicRanges::sort(c(snp.pos, cn.pos))
}

#' get SNP signals from SignalSet
#'
#' @param sset SignalSet
#' @return GRanges with signals
#' @import GenomicRanges
#' @export
sset.snp.signal <- function(sset) {
  snp.pos <- getBuiltInData('snp6.snp.pos')
  GenomicRanges::mcols(snp.pos)$signalA <- sset$snp.median[match(paste0(names(snp.pos), '_', snp.pos$alleleA), names(sset$snp.median))]
  GenomicRanges::mcols(snp.pos)$signalB <- sset$snp.median[match(paste0(names(snp.pos), '_', snp.pos$alleleB), names(sset$snp.median))]
  snp.pos
}
