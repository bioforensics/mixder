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
checking_af = function(affile) {
  if (grepl(".rda", affile, ignore.case = TRUE)) {
    load(affile)
  } else {
    af=data.frame(fread(affile, header=T, sep=","))
    filename = gsub(".csv","", affile)
    if (!isTruthy(af[c(1:4),1] == c("A", "C", "G", "T"))) {
      af_formatted = format_af(af)
      utils::write.csv(af_formatted, glue("{filename}_EFMformatted.csv"), row.names=F, quote=F)
    } else {
      af_formatted = af
    }
    popFreq=read_in_freq(af_formatted)
    save(popFreq, file=glue("{filename}.rda"))
  }
  return(popFreq)
}
