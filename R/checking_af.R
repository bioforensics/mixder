
#' Checking AF file for correct format; formatting if necessary
#'
#' @param affile Allele Frequency file
#' @param outpath Path to write formatted AF file to if necessary
#'
#' @return Frequency file loaded for EFM
#' @export
checking_af = function(affile, outpath) {
  af=utils::read.csv(affile, header=T)
  if (isTruthy(af[c(1:4),1] == c("A", "C", "G", "T"))) {
    finalfreq = euroformix::freqImport(affile)[[1]]
  } else {
    af_formatted = format_af(af)
    utils::write.csv(af_formatted, glue("{outpath}/custom_AF_formatted.csv"), row.names=F, quote=F)
    finalfreq = euroformix::freqImport(glue("{outpath}/custom_AF_formatted.csv"))[[1]]
  }
  return(finalfreq)
}
