# -------------------------------------------------------------------------------------------------
# Copyright (c) 2026, DHS.
#
# This file is part of MixDeR and is licensed under the BSD license: see LICENSE.
#
# This software was prepared for the Department of Homeland Security (DHS) by the Battelle National
# Biodefense Institute, LLC (BNBI) as part of contract HSHQDC-15-C-00064 to manage and operate the
# National Biodefense Analysis and Countermeasures Center (NBACC), a Federally Funded Research and
# Development Center.
# -------------------------------------------------------------------------------------------------

#' Convert frequency file name
#'
#' @param freq_id frequency file name to convert
#'
#' @returns string of corrected name
#' @export
#'
decode_freq_name = function(freq_id) {
  if (grepl("1000g", freq_id, fixed=TRUE) & grepl("global", freq_id, fixed=TRUE)) {
    return("Global - 1000G")
  } else if (grepl("gnomad", freq_id, fixed=TRUE)) {
    return("Global - gnomAD")
  } else if (grepl("afr", freq_id, fixed=TRUE)) {
    return("AFR - 1000G")
  } else if (grepl("amr", freq_id, fixed=TRUE)) {
    return("AMR - 1000G")
  } else if (grepl("eas", freq_id, fixed=TRUE)) {
    return("EAS - 1000G")
  } else if (grepl("eur", freq_id, fixed=TRUE)) {
    return("EUR - 1000G")
  } else if (grepl("sas", freq_id, fixed=TRUE)) {
    return("SAS - 1000G")
  } else {
    return(freq_id)
  }
}
