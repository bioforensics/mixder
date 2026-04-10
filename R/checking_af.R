# -------------------------------------------------------------------------------------------------
# Copyright (c) 2024, DHS.
#
# This file is part of MixDeR and is licensed under the BSD license: see LICENSE.
#
# This software was prepared for the Department of Homeland Security (DHS) by the Battelle National
# Biodefense Institute, LLC (BNBI) as part of contract HSHQDC-15-C-00064 to manage and operate the
# National Biodefense Analysis and Countermeasures Center (NBACC), a Federally Funded Research and
# Development Center.
# -------------------------------------------------------------------------------------------------
#' Checking AF file for correct format; formatting if necessary
#'
#' @param affile Allele Frequency file
#' @param contrib contributor assigned to AF file
#'
#' @return Frequency file loaded for EFM
#' @export
#'
#' @importFrom data.table fread
checking_af = function(affile, contrib) {
  af=data.frame(fread(affile, header=T, sep=","))
  if (isTruthy(af[c(1:4),1] == c("A", "C", "G", "T"))) {
    #finalfreq = euroformix::freqImport(affile)[[1]]
    finalfreq = read_in_freq(affile)
  } else {
    af_formatted = format_af(af)
    outpath = dirname(af)
    utils::write.csv(af_formatted, glue("{outpath}/custom_AF_{contrib}_formatted.csv"), row.names=F, quote=F)
    #finalfreq = euroformix::freqImport(glue("{outpath}/custom_AF_{contrib}_formatted.csv"))[[1]]
    finalfreq = read_in_freq(glue("{outpath}/custom_AF_{contrib}_formatted.csv"))
  }
  return(finalfreq)
}
